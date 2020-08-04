(ns kcats.core
  (:require [clojure.spec.alpha :as spec]
            [expound.alpha :as expound])
  (:refer-clojure :exclude [eval]))

(set! spec/*explain-out* (expound/custom-printer
                          {:show-valid-values? true}))
(spec/check-asserts true)

(spec/def ::boolean boolean?)
(spec/def ::number number?)
(spec/def ::string string?)
(spec/def ::bytes bytes?)

(expound/def ::item
  (spec/or :aggregate ::aggregate
           :boolean ::boolean
           :number ::number
           :string ::string
           :bytes ::bytes
           :word ::word))

(expound/def ::aggregate (spec/coll-of ::item :kind vector?) "aggregate?")
(expound/def ::program ::aggregate)

(def word? symbol?)
(spec/def ::word word?)

(spec/def ::axiom-definition (spec/keys :req [::fn ::spec]))
(spec/def ::program-definition (spec/keys :req [::fn ::definition]))

(spec/def ::dictionary (spec/map-of ::word (spec/or ::axiom-definition
                                                    ::program-definition)))

(def stack? (every-pred (complement indexed?) sequential?))

(spec/def ::stack (spec/coll-of ::item
                                :kind stack?))

(spec/def ::expression (spec/coll-of ::item
                                     :kind stack?))

(spec/def ::environment (spec/keys :req [::stack ::dictionary ::expression]))

(declare eval-step) ;; we'll define this later

(defn eval
  [env]
  (->> env
       (iterate eval-step)
       (drop-while (comp seq ::expression))
       first))

(defmulti onto-stack
  "Evaluate one item in the given environment. A single step in a
  program's execution."
  (fn [{[item] ::expression}]
    (type item)))

(defn- push
  "Push an item from the expression into the stack"
  [{[item & others] ::expression :as env}]
  (-> env
      (assoc ::expression others)
      (update ::stack conj item)))

(defmethod onto-stack Number [env]
  (push env))

(defmethod onto-stack String [env]
  (push env))

(defmethod onto-stack Boolean [env]
  (push env))

;; PersistentVector is the clojure type for an aggregate
(defmethod onto-stack clojure.lang.PersistentVector [env]
  (push env))

(defmethod onto-stack (Class/forName "[B") [env]
  (push env))

(defmethod onto-stack clojure.lang.Symbol [{[word & others] ::expression
                                            ::keys [dictionary stack]
                                            :as env}]
  (let [{f ::fn spec ::spec} (dictionary word)]
    (when spec
      (spec/assert spec stack))
    (if f
      (f (update env ::expression rest))
      (push env))))

(defn definition
  "Returns the definition of an item, if it's a word defined in terms
  of other words"
  [{::keys [dictionary] :as env} item]
  (some-> dictionary (get item) ::definition))

(defn eval-step
  "Evaluate one step in the environment."
  [{[next-item & items :as expression] ::expression ::keys [stack dictionary] :as env}]
  (apply println (concat (reverse stack) "." expression))
  (if-let [d (definition env next-item)]
    (assoc env ::expression (concat (list* d) items)) ;; replace item with definition
    ;; eval the thing onto the stack
    (onto-stack env)))

(defn update-stack [f env]
  (update env ::stack f))

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
 [new-word program]"
  [{[[word word-def] & others] ::stack
    ::keys [expression dictionary]}]
  {::stack others
   ::expression (pop expression)
   ::dictionary (assoc dictionary word {::definition word-def})})

(spec/def ::stack-min-depth-1 (spec/coll-of ::item
                                            :kind stack?
                                            :min-count 1))
(spec/def ::stack-min-depth-2 (spec/coll-of ::item
                                            :kind stack?
                                            :min-count 2))

(spec/def ::binary-arithmetic (spec/cat :x ::number
                                        :y ::number
                                        :others (spec/* ::item)))

(spec/def ::predicate (spec/cat :x ::item
                                :others (spec/* ::item)))

(spec/def ::quoted-word (spec/coll-of ::word
                                      :count 1
                                      :kind vector?))
(def arithmetic-words
  (into {} cat
        [(for [sym ['+ '- '/ '* '< '<= '> '>= 'min 'max]]
           [sym
            {::spec ::binary-arithmetic
             ::fn (f-stack 2 (resolve sym))}])
         (for [sym ['inc 'dec]]
           [sym
            {::spec (spec/cat :x ::number
                              :others (spec/* ::item))
             ::fn (f-stack 1 (resolve sym))}])]))

(def predicates
  (into {} cat
        [(for [sym ['odd? 'even? 'sequential? 'zero? 'pos? 'neg?
                    'number? 'int? 'true? 'false? 'nil? 'some?
                    'string? 'empty?]]
           [sym {::spec ::predicate
                 ::fn (f-stack 1 (resolve sym))}])
         (for [sym ['starts-with? 'ends-with?]]
           [sym {::spec ::stack-min-depth-2
                 ::fn (f-stack 2 (resolve sym))}])]))

(def cardinality
  (into {} cat
        [(for [[word num] {'first 1
                           'second 2
                           'third 3
                           'fourth 4
                           'fifth 5
                           'sixth 6
                           'seventh 7
                           'eighth 8
                           'ninth 9
                           'tenth 10}]
           [word {::spec (spec/coll-of ::item
                                       :kind stack?)
                  ::definition [num]}])]))
(defn roll*
  [[distance depth & others]]
  (let [[top rest] (split-at depth others)
        [a b] (split-at (mod (- distance) depth) top)]
    (concat b a rest)))

(defn clone*
  [[depth & others]]
  (let [[top rest] (split-at depth others)]
    (concat top (conj rest (last top)))))

(defn discard*
  [[depth & others]]
  (let [[top [_ & rest]] (split-at (dec depth) others)]
    (concat top rest)))

(defn dip*
  [{[depth p & others] ::stack
    expression ::expression
    dictionary ::dictionary}]
  (let [[top rest] (split-at depth others)]
    {::stack rest
     ::expression (concat p (reverse top) expression)
     ::dictionary dictionary}))

(defn recur*
  [{[rec2 rec1 & others] ::stack
    expression ::expression
    dictionary ::dictionary}]
  (let [[then pred] others
        f [pred then rec1 rec2 'genrec]
        else (vec (concat rec1 [f] rec2))]
    {::stack (conj others else)
     ::expression (conj expression 'ifte)
     ::dictionary dictionary}))

(defn spread*
  "Runs programs with the ability to undo stack effects, also adds
  potential parallelism (not implemented yet). Takes two programs,
  `spread` and `gather`. The `spread` program will be run on the
  current stack, and it should leave an aggregate `programs` on
  top. For each `program` in `programs`, a new temporary environment
  will be created, with the original stack (minus `spread` and
  `gather`), and `program` executed on it. Then all the `tops` of
  those environments' stacks will be gathered up into an aggregate and
  put on another fresh original stack (again without the original top
  two items), and the `gather` program will be executed on it."
  [{[gather spread & others] ::stack
    expression ::expression
    dictionary ::dictionary
    :as env}]
  (let [base-env (assoc env ::stack others)
        spread-env (update base-env ::expression concat spread)
        programs (-> spread-env eval ::stack first)
        tops (for [program programs]
               (-> base-env
                   (update ::expression concat program)
                   eval
                   ::stack
                   ::first))]
    {::stack (conj others tops)
     ::expression (concat expression gather)
     ::dictionary dictionary}))

(def builtin-words
  (merge
   arithmetic-words
   predicates
   cardinality
   {'discard {::spec ::stack-min-depth-1
              ::fn (partial update-stack #'discard*)}
    'pop {::spec ::stack-min-depth-1
          ::definition '[1 discard]}
    'clone {::spec ::stack-min-depth-1
            ::fn (partial update-stack #'clone*)}
    'swap {::spec ::stack-min-depth-2
           #_::fn #_(partial update-stack (fn [[a b & others]]
                                            (-> others
                                                (conj a)
                                                (conj b))))
           ::definition '[2 1 roll]}
    'roll {::spec (spec/coll-of ::item
                                :kind stack?
                                :min-count 2) ;; should be dynamic w depth
           ::fn (partial update-stack #'roll*)}
    'execute {::spec (spec/cat :program ::program
                               :others (spec/* ::item))
              ::fn (fn [{[p & others] ::stack ::keys [dictionary expression]}]
                     {::stack others
                      ::expression (concat p expression)
                      ::dictionary dictionary})}
    'dip {::spec (spec/cat :depth integer?
                           :program ::aggregate
                           :others (spec/* ::item))
          ::fn #'dip*}
    'quote {::spec (spec/cat :item ::item
                             :others (spec/* ::item))
            ::fn (partial update-stack (fn [[item & others]]
                                         (concat others [[item]])))}
    'inscribe {::spec (spec/cat :definition (spec/tuple ::word ::aggregate)
                                :others (spec/* ::item))
               ::fn #'inscribe}
    'describe {::spec (spec/cat :word (spec/tuple ::word)
                                :others (spec/* ::item))
               ::fn (fn [{[[word] & others] ::stack dict ::dictionary :as env}]
                      (let [dfn (-> dict (get word) ::definition)]
                        (if dfn
                          (assoc env ::stack (conj others dfn))
                          (-> "Word %s is not defined"
                              (format word)
                              Exception.
                              throw))))}
    'branch {::spec (spec/cat :false-branch ::aggregate
                              :true-branch ::aggregate
                              :condition ::item
                              :others (spec/* ::item))
             ::fn (fn [{[f t b & others] ::stack expression ::expression :as env}]
                    (-> env
                        (assoc ::stack others)
                        (update ::expression into (conj (if b t f)))))}
    'ifte {::spec (spec/cat :false-branch ::aggregate
                            :true-branch ::aggregate
                            :condition ::program
                            :others (spec/* ::item))
           ::definition '[[execute] second dip branch]}
    'step {::spec (spec/cat :program ::aggregate
                            :aggregate ::aggregate
                            :others (spec/* ::item))
           ::fn (fn [{[p [agg-item & agg-rest] & others] ::stack
                      ::keys                             [expression dictionary]}]
                  {::expression (concat (cond-> ['execute]
                                          (seq agg-rest) (concat [(vec agg-rest) p 'step]))
                                        expression)
                   ::stack (conj others agg-item p)
                   ::dictionary dictionary})}
    'recur {::spec (spec/cat :rec2 ::program
                             :rec1 ::program
                             :true-branch ::program
                             :false-branch ::program
                             :others (spec/* ::item))
            ::fn #'recur*}
    'spread {::spec (spec/cat :gather ::program
                              :spread ::program
                              :others (spec/* ::item))
             ::fn #'spread*} 
    '= {::spec (spec/cat :x ::item
                         :y ::item
                         :other (spec/* ::item))
        ::fn (f-stack 2 =)}

    'cons {::spec (spec/cat :x ::item
                            :aggregate ::aggregate
                            :others (spec/* ::item))
           ::fn (f-stack 2 (fn [x a]
                             (conj (vec a) x)))}
    'uncons {::spec (spec/cat :aggregate ::aggregate
                              :others (spec/* ::item))
             ::fn (partial update-stack (fn [[[x & rest] & others]]
                                          (apply list x (vec rest) others)))}
    'concat {::spec (spec/cat :agg1 ::aggregate
                              :agg1 ::aggregate
                              :others (spec/* ::item))
             ::fn (f-stack 2 (comp vec #(concat %2 %1)))}
    'swaack {::spec (spec/cat :list ::aggregate
                              :others (spec/* ::item))
             ::doc "Swaps the list on top of the stack and the rest of the stack"
             ::fn (fn [{[a & others] ::stack ::keys [dictionary expression]}]
                    {::stack (apply list (vec others) a)
                     ::expression expression
                     ::dictionary dictionary})}
    'infra {::spec (spec/cat :program ::program
                             :list ::aggregate
                             :others (spec/* ::item))
            ::doc "Accept a quoted program and a list on the stack and run the program
                    with the list as its stack.  Does not affect the rest of the stack."
            ::definition '[swap swaack uncons dip swaack]}
    'some {::spec (spec/cat :program ::aggregate
                            :aggregate ::aggregate ;; TODO finish this
                            :others (spec/* ::item))
           ::fn (fn [env]
                  (update-stack
                   (fn [[p a & others :as stack]])))}
    'getfirst {::spec (spec/cat :aggregate ::aggregate, :other (spec/* ::item))
               ::fn (f-stack 1 first)}
    'map {::spec (spec/cat :program ::aggregate
                           :aggregate ::aggregate
                           :others (spec/* ::item))
          ;; runs a parallel simulation - if the map function
          ;; tries to add or remove more stack elements those
          ;; changes will be lost - only top stack element is
          ;; collected from each parallel run of p
          ::fn (fn [env]
                 #_(update-stack (fn [[p a & others :as stack]]
                                   (conj others
                                         (->> (for [item a]
                                                (eval (with-stack env (conj others item)) p))
                                              (map (comp first :stack))
                                              (into []))))
                                 env))}
    'filter {::spec (spec/cat :program ::aggregate
                              :aggregate ::aggregate
                              :others (spec/* ::item))
             ;; runs a parallel simulation - if the filter function
             ;; tries to add or remove more stack elements those
             ;; changes will be lost - only top stack element is
             ;; collected from each parallel run of p
             ::fn (fn [env]
                    #_(update-stack (fn [[p a & others :as stack]]
                                      (->> a
                                           (filter #(leaves-true? (with-stack env (conj others %)) p))
                                           (into [])
                                           (conj others)
                                           (into [])))
                                    env))}

    'every? {::spec (spec/cat :program ::aggregate
                              :aggregate ::aggregate
                              :others (spec/* ::item))
             ::fn (fn [env]
                    #_(update-stack (fn [[p a & others :as stack]]
                                      (->> a
                                           (every? #(leaves-true? (with-stack env (conj others %)) p))
                                           (conj others)))
                                    env))}
    'and  {::spec (spec/cat :x ::item, :y ::item,
                            :others (spec/* ::item))
           ::fn (f-stack 2 #(and %1 %2))}
    'or  {::spec (spec/cat :x ::item, :y ::item,
                           :others (spec/* ::item))
          ::fn (f-stack 2 #(or %1 %2))}
    'in  {::spec (spec/cat :aggregate ::aggregate, :item ::item
                           :other (spec/* ::item))
          ::fn (f-stack 2 contains?)}
    'intersection {::spec (spec/cat :aggregate-x ::aggregate, :aggregate-y ::aggregate,
                                    :others (spec/* ::item))
                   ::fn (f-stack 2 (fn [x y]
                                     (into []
                                           (clojure.set/intersection
                                            (into #{} x)
                                            (into #{} y)))))}
    'trace {::spec (spec/cat :program ::aggregate
                             :others (spec/* ::item))
            ::fn (fn [{[p & others] :stack :as env}]
                   #_(reduce (fn [env item]
                               (let [r (eval-one env item)]
                                 (debug r item)
                                 r))
                             (with-stack others)
                             p))}
    'multi {::spec (spec/cat :dispatch ::program
                             :others (spec/* ::item))
            ::definition '[quote [execute [] swap get execute] concat]}}))

(defn k
  "Run a program with the default env and return the result"
  [p]
  (::stack (eval {::stack '()
                  ::dictionary builtin-words
                  ::expression (list* p)})))

(spec/def ::pair (spec/coll-of ::item :kind vector? :count 2))

(spec/def ::association-list (spec/coll-of ::pair
                                           :kind vector?))

(def builtin-words
  (update builtin-words merge
          {'assoc {::spec (spec/cat :item ::pair, :alist ::association-list
                                    :others (spec/* ::item))
                   ::fn (partial update-stack (fn [[item alist & others]]
                                                (->> alist
                                                     (into {})
                                                     (apply assoc)
                                                     (into [])
                                                     (conj others))))}}))


