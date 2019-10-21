(ns kcats.core
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound])
  (:refer-clojure :exclude [eval]))

;; based on Joy concatenative language

;; specs
(expound/def ::aggregate vector? "aggregate?")
(s/def ::boolean boolean?)
(s/def ::number number?)
(s/def ::string string?)
(s/def ::bytes bytes?)
(expound/def ::non-empty (complement seq) "not-empty?")
(expound/def ::word keyword? "word?")
(expound/def ::stack-item (s/or :aggregate ::aggregate
                                :boolean ::boolean
                                :number ::number
                                :string ::string
                                :bytes ::bytes
                                :word ::word))
(s/def ::stack (s/coll-of ::stack-item
                          :kind list?))
(s/def ::stack-min-depth-1 (s/coll-of ::stack-item
                                      :kind list?
                                      :min-count 1))
(s/def ::stack-min-depth-2 (s/coll-of ::stack-item
                                      :kind list?
                                      :min-count 2))
(s/def ::definition (s/spec (s/cat :new-word ::word
                                   :program (s/* ::stack-item))))
(s/def ::binary-arithmetic (s/cat :x ::number
                                  :y ::number
                                  :others (s/* ::stack-item)))

(s/def ::quoted-word (s/coll-of ::word
                                :count 1
                                :kind vector?))

(set! s/*explain-out* (expound/custom-printer
                       {:show-valid-values? true}))
(s/check-asserts true)

(defrecord Env [stack dict])

(defmulti eval-one
  "Evaluate one item in the given environment. A single step in a
  program's execution."
  (fn [env item]
    (type item)))

(def eval (partial reduce eval-one))

(defn- push [env item]
  (update env :stack conj item))

(defn update-stack [f env]
  (update env :stack f))

(defn s-apply
  "Take nitems off the stack, apply them to f. f should return a list
   to put back onto the stack."
  [nitems f stack]
  (let [[a b] (split-at nitems stack)]
    (into b (apply f a))))

(defn s-apply-one
  "Like s-apply, but useful for functions where you only want to put
   one item back on the stack and f doesn't return a
   list. Automatically creates a list of one item."
  [nitems f stack]
  (s-apply nitems (comp list f) stack))

(defn f-stack
  "Apply nitems from stack to f, put result back on stack"
  [nitems f]
  (partial update-stack (partial s-apply-one nitems f)))

(defn inscribe
  "Define a new word. Takes an aggregate off the stack which should be of the
   form:
   [:new-word [item1 item2 ...]]"
  [{:keys [stack] :as env}]
  (let [[[word word-def] & others] stack]
    (assoc (assoc-in env [:dict word]
                     {:fn (fn [env]
                            (reduce eval-one env word-def))
                      :definition word-def})
           :stack (or others '()))))

(def arithmetic-words
  (into {} cat
        [(for [sym ['+ '- '/ '* '< '<= '> '>= 'min 'max]]
           [(keyword (name sym))
            {:spec ::binary-arithmetic
             :fn (partial update-stack
                          (partial s-apply-one 2 (resolve sym)))}])
         (for [sym ['inc 'dec]]
           [(keyword (name sym))
            {:spec (s/cat :x ::number
                          :others (s/* ::stack-item))
             :fn (partial update-stack
                          (partial s-apply-one 1 (resolve sym)))}])]))

(defn debug [env item]
  (println item (:stack env)))

(def comparator-words)
(def builtin-words
  (merge
   arithmetic-words
   {:pop {:spec ::stack-min-depth-1
          :fn (partial update-stack pop)}
    :dup {:spec ::stack-min-depth-1
          :fn (partial update-stack (fn [s]
                                      (conj s (first s))))}
    :swap {:spec ::stack-min-depth-2
           :fn (partial update-stack (fn [[a b & others]]
                                       (-> others
                                           (conj a)
                                           (conj b))))}
    :i {:spec (s/cat :program ::aggregate
                     :others (s/* ::stack-item))
        :fn (fn [env]
              (let [[a & others] (:stack env)]
                (eval (assoc env :stack others) a)))}
    :dip {:spec (s/cat :program ::aggregate
                       :x ::stack-item
                       :others (s/* ::stack-item))
          :fn (fn [env]
                (let [[p x & others] (:stack env)]
                  (eval-one (assoc env :stack
                                   (conj others
                                         (conj p x))) :i)))}
    :inscribe {:spec (s/cat :definition ::definition
                            :others (s/* ::stack-item))
               :fn #'inscribe}
    :describe {:spec (s/cat :word ::quoted-word
                            :others (s/* ::stack-item))
               :fn (fn [{:keys [stack dict] :as env}]
                     (let [[[word] & others] stack
                           dfn (-> dict (get word) :definition)]
                       (if dfn
                         (update-stack #(-> %
                                            pop
                                            (conj dfn)) env)
                         (-> "Word %s is not defined"
                             (format word)
                             Exception.
                             throw))))}
    :branch {:spec (s/cat :condition ::boolean
                          :true-branch ::aggregate
                          :false-branch ::aggregate
                          :others (s/* ::stack-item))
             :fn (fn [{:keys [stack] :as env}]
                   (let [[b t f & others] stack]
                     (eval (assoc env :stack others)
                           (if b t f))))}
    := {:spec (s/cat :x ::stack-item
                     :y ::stack-item
                     :other (s/* ::stack-item))
        :fn (f-stack 2 =)}
    :cons {:spec (s/cat :x ::stack-item
                        :aggregate ::aggregate
                        :others (s/* ::stack-item))
           :fn (f-stack 2 cons)}
    :some {:spec (s/cat :aggregate ::aggregate ;; TODO finish this
                        :program ::aggregate
                        :others (s/* ::stack-item))
           :fn (fn [env]
                 (update env :stack
                         (fn [[a p & others :as stack]]
                           )))}
    :first {:spec (s/cat :aggregate ::aggregate, :other (s/* ::stack-item))
            :fn (f-stack 1 first)}
    :map {:spec (s/cat :aggregate ::aggregate
                       :program ::aggregate
                       :others (s/* ::stack-item))
          :fn (fn [env]
                ;; runs a parallel simulation - if the map function
                ;; tries to add or remove more stack elements those
                ;; changes will be lost - only top stack element is
                ;; collected from each parallel run of p
                (update env :stack
                        (fn [[a p & others :as stack]]
                          (conj others
                                (->> (for [item a]
                                       (eval (assoc env :stack (conj others item)) p))
                                     (map (comp first :stack))
                                     (into []))))))}
    :filter {:spec (s/cat :aggregate ::aggregate
                          :program ::aggregate
                          :others (s/* ::stack-item))
             :fn (fn [env]
                   ;; runs a parallel simulation - if the filter function
                   ;; tries to add or remove more stack elements those
                   ;; changes will be lost - only top stack element is
                   ;; collected from each parallel run of p
                   (update env :stack
                           (fn [[a p & others :as stack]]
                             (->>
                              (for [item a
                                    :when (-> (eval (assoc env :stack
                                                           (conj others item))
                                                      p)
                                              :stack first true?)]
                                item)
                              (into [])
                              (conj others)))))}
    :and  {:spec (s/cat :x ::stack-item, :y ::stack-item,
                        :others (s/* ::stack-item))
           :fn (f-stack 2 #(and %1 %2))}
    :or  {:spec (s/cat :x ::stack-item, :y ::stack-item,
                       :others (s/* ::stack-item))
          :fn (f-stack 2 #(or %1 %2))}
    :in  {:spec (s/cat :aggregate ::aggregate, :item ::stack-item
                       :other (s/* ::stack-item))
          :fn (f-stack 2 contains?)}
    :intersection {:spec (s/cat :aggregate-x ::aggregate, :aggregate-y ::aggregate,
                                :others (s/* ::stack-item))
                   :fn (f-stack 2 (fn [x y]
                                    (into []
                                          (clojure.set/intersection
                                           (into #{} x)
                                           (into #{} y)))))}
    :trace {:spec (s/cat :program ::aggregate
                         :others (s/* ::stack-item))
            :fn (fn [{[p & others] :stack :as env}]
                  (reduce (fn [env item]
                            (let [r (eval-one env item)]
                              (debug r item)
                              r))
                          (assoc env :stack others)
                          p))}}))

(def core
  (let [words [[:swapd [:swap :dip]]
               [:dupd [:dup :dip]]
               [:popd [:pop :dip]]
               [:swons [:swap :cons]]]]
    (->> :inscribe repeat (interleave words) (into []))))

(defmethod eval-one Number [env item]
  (push env item))

(defmethod eval-one String [env item]
  (push env item))

(defmethod eval-one Boolean [env item]
  (push env item))

(defmethod eval-one clojure.lang.PersistentVector [env item]
  (push env item))

(defmethod eval-one (Class/forName "[B") [env item]
  (push env item))

(defmethod eval-one clojure.lang.Keyword [env word]
  (let [{f :fn spec :spec} (-> env :dict word)]
    (when spec
      (s/assert spec (:stack env)))
    (if f
      (f env)
      (throw (ex-info (format "Undefined word %s" word)
                      {:word word
                       :env env})))))

(defn ev
  "Evaluate a program given an env in an atom, updates the atom and
   returns the new env"
  [state items]
  (swap! state eval items))

(defn default-env
  "Returns a fresh environment with just core words"
  []
  (eval (->Env (list) builtin-words) core))

(defn new-env
  ([builtin-words core]
   (let [env (->Env (list) builtin-words)]
     (eval env core))))

(defn k
  "Evaluates program in a fresh env, returns the resulting stack"
  [p]
  (->> p (eval (default-env)) :stack))

#_(defn threshold
  [scripts threshold](let [scripts [:a :b :c]](filter (fn []))))

