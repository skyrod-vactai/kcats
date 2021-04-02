(ns kcats.core
  (:require [clojure.spec.alpha :as spec]
            [expound.alpha :as expound])
  (:refer-clojure :exclude [eval test]))

(set! spec/*explain-out* (expound/custom-printer
                          {:show-valid-values? true}))
(spec/check-asserts true)

(expound/def ::boolean boolean?)
(expound/def ::number number?)
(expound/def ::string string?)
(expound/def ::bytes bytes?)
(expound/def ::integer integer?)

(in-ns 'kcats.core)

(expound/def ::value
  (spec/or :value-list ::value-list
           :boolean ::boolean
           :number ::number
           :string ::string
           :bytes ::bytes
           :word ::word))

(expound/def ::item (constantly true))

(expound/def ::list (spec/coll-of ::item :kind vector?) "list?")
(expound/def ::value-list (spec/coll-of ::value :kind vector?) "value-list?")
(expound/def ::program ::list)

(def word? symbol?)
(expound/def ::word word?)

(expound/def ::axiom-definition (spec/keys :req [::fn ::spec]))
(expound/def ::program-definition (spec/keys :req [::definition]))

(expound/def ::dictionary (spec/map-of ::word
                                       (spec/or :axiom ::axiom-definition
                                                :user-defined ::program-definition)))

(def stack? (every-pred (complement indexed?) sequential?))

(expound/def ::stack (spec/coll-of ::item
                                   :kind stack?))

(expound/def ::expression (spec/coll-of ::item
                                        :kind stack?))

(expound/def ::environment (spec/keys :req [::stack ::dictionary ::expression]))

;; since we'll commonly be defining specs for a stack, we'll nearly
;; always not care about anything below a certain depth, so let's make
;; a macro that encapsulates that.
(defmacro stack-spec
  [& args]
  `(spec/cat ~@args
             :others (spec/* ::item)))

(declare eval-step) ;; we'll define this later

(defn eval
  ([{:keys [max-steps before-step]} env]
   (let [eval-step (if before-step
                     (comp eval-step before-step)
                     eval-step)]
     (loop [{::keys [expression] :as env} env
            step-count 0]
       (if (or (not (seq expression)) ;; nothing left to run
               (and max-steps (>= step-count max-steps)))
         env
         (recur (eval-step env) (inc step-count))))))
  ([env]
   (eval {} env)))

(defmulti onto-stack
  "Evaluate one item in the given environment. A single step in a
  program's execution."
  (fn [{[item] ::expression}]
    (type item)))

(defmethod onto-stack :default
  [{[item & others] ::expression :as env}]
  (-> env
      (assoc ::expression others)
      (update ::stack conj item)))

(in-ns 'kcats.core)
(require '[clojure.string :as string])

(defn invoke*
  [{[word & others] ::expression
    ::keys [dictionary stack]
    :as env}]
  {::stack
   (let [[_ arity fn-name] (->> word str (re-find #"(\.+)(.*)\.?"))
         [args others] (split-at (count arity) stack)
         args (reverse args)] ;; due to stacking reversing the args previously
     (conj others
           (cond
             (not fn-name) word

             (.endsWith fn-name ".") ;; java constructor
             (clojure.lang.Reflector/invokeConstructor
              (Class/forName (->> fn-name
                                  count
                                  dec
                                  (subs fn-name 0)))
              (into-array Object args))

             (and (.startsWith fn-name "+")
                  (.contains fn-name "/")) ;; static method
             (let [[clazz method] (-> fn-name
                                      (subs 1)
                                      (string/split #"/"))]
               (clojure.lang.Reflector/invokeStaticMethod clazz method
                                                          (into-array Object args)))
             
             (.startsWith fn-name "+") ;; instance method
             (clojure.lang.Reflector/invokeInstanceMethod
              (last args)
              (subs fn-name 1)
              (into-array Object (butlast args)))

             ;; clojure function
             (some-> fn-name symbol resolve) (-> fn-name
                                                 symbol
                                                 resolve
                                                 (apply args))
             :else (throw (Exception. (str "Cannot resolve platform word: " word))))))
   ::expression others
   ::dictionary dictionary})

(defmethod onto-stack clojure.lang.Symbol [{[word & others] ::expression
                                            ::keys [dictionary stack]
                                            :as env}]
  (let [{f ::fn spec ::spec} (dictionary word)]
    (when spec
      (spec/assert spec stack))
    (cond
      f (f (update env ::expression rest)) ;; drop this word now that we've used it
      :else (invoke* env))))

(defn definition
  "Returns the definition of an item, if it's a word defined in terms
  of other words"
  [{::keys [dictionary] :as env} item]
  (some-> dictionary (get item) ::definition))

(defn eval-step
  "Evaluate one step in the environment."
  [{[next-item & items :as expression] ::expression ::keys [stack dictionary] :as env}]
  (if-let [d (definition env next-item)]
    (assoc env ::expression (concat (list* d) items)) ;; replace item with definition
    
    ;; eval the thing onto the stack
    (onto-stack env)))
 
(defn print-env
  "Prints the expression/stack and then return env"
  [{::keys [expression stack] :as env}]
  (apply pr (reverse expression))
  (print " . ")
  (apply prn stack)
  env)

(def nothing [])

(defn nothing?
  [x]
  (= nothing x))

(defn nothing->nil
  "Returns argument unless it's a kcats 'nothing' (empty list), in
  which case it returns nil"
  [x]
  (if (nothing? x)
    nil
    x))

(defn nil->nothing
  "Returns argument unless it's nil, in which case it returns empty list"
  [x]
  (if (nil? x)
    nothing
    x))

(in-ns 'kcats.core)

(defn f-stack
  "Apply nitems from stack to f, put result back on stack after
   dropping nitems-drop."
  ([nitems-use nitems-drop f]
   (fn [env]
     (update env ::stack
             (fn [stack]
               (let [use (take nitems-use stack)
                     stack (drop nitems-drop stack)]
                 ;; items are reversed so that we can write code with arguments in
                 ;; the same order as we'd use in other langs, eg so that [2 1 >]
                 ;; -> true
                 ;;
                 ;; The top item in the stack would normally be the first
                 ;; arg, but that was the LAST arg written in the code.
                 (conj stack (apply f (reverse use))))))))
  ([nitems f]
   (f-stack nitems nitems f)))

(defn env-effect
  "Calls f with nitems from stack, it should return a pair (items to
  prepend to the expression, items to replace the nitems with)"
  [nitems f]
  (fn [{::keys [stack expression dictionary]}]
    (let [[a b] (split-at nitems stack)
          [new-expression-items new-stack-items] (apply f a)]
          {::stack (concat new-stack-items b)
           ::expression (concat new-expression-items expression)
           ::dictionary dictionary})))

(defmacro effect
  "Constructs a fn with given in args, whose body is out. That
  function body should return a pair (new-expression-items,
  new-stack-items)"
  [in out]
  `(env-effect ~(count in) (fn ~in ~out)))

(defmacro stack-effect
  "Takes a stack effect notation and turns it into a call to
  env-effect"
  [in out]
  `(env-effect ~(count in) (fn ~in [[] ~out])))

(in-ns 'kcats.core)

(defonce core-words (atom {}))

(def arithmetic-words
  (into {} cat
        [(for [sym ['+ '- '/ '* '< '<= '> '>= 'min 'max 'quot 'rem 'mod]]
           [sym
            {::spec (stack-spec :x ::number
                                :y ::number)
             ::fn (f-stack 2 (resolve sym))}])
         (for [sym ['inc 'dec]]
           [sym
            {::spec (stack-spec :x ::number)
             ::fn (f-stack 1 (resolve sym))}])]))

;; need to implement `some` to respect empty list being falsey
(def predicates
  (into {} cat
        [(for [sym ['odd? 'even? 'sequential? 'zero? 'pos? 'neg?
                    'number? 'int? 'true? 'false?
                    'string? 'empty?]]
           [sym {::spec (stack-spec :x ::item)
                 ::fn (f-stack 1 (resolve sym))}])
         (for [sym ['starts-with? 'ends-with?]]
           [sym {::spec (stack-spec :subject ::item
                                    :object ::item)
                 ::fn (f-stack 2 (resolve sym))}])]))

(def axiom-words
  {'discard {::spec (stack-spec :a ::item)
             ::fn (stack-effect [a] [])
             ::examples '[[[1 2 3 discard] [2 1]]
                          [[1 2 3 [a b c] discard] [3 2 1]]]}
   'clone {::spec (stack-spec :a ::item)
           ::fn (stack-effect [a] [a a])
           ::examples '[[[1 2 3 clone] [3 3 2 1]]]}
   'swap {::spec (stack-spec :a ::item, :b ::item)
          ::fn (stack-effect [a b] [b a])
          ::examples '[[[1 2 3 swap] [2 3 1]]]}
   ;; TODO: this is just [swap] dip. worth having its own word?
   'swapdown {::spec (stack-spec :a ::item, :b ::item, :c ::item)
              ::fn (stack-effect [a b c] [a c b])
              ::examples '[[[1 2 3 swapdown] [3 1 2]]]}
   'rotate {::spec (stack-spec :a ::item, :b ::item, :c ::item)
            ::fn (stack-effect [a b c] [c a b])}
   'execute {::spec (stack-spec :p ::program)
             ::fn (effect [p] [p []])
             ::examples '[[[[1 2 +] execute] [3]]
                          [[2 [+] 4 swap execute] [6]]]}
   'dip {::spec (stack-spec :p ::program
                            :a ::item)
         ::fn (effect [p a] [(conj (vec p) a) []])
         ::examples '[[[1 8 [inc] dip] [8 2]]]}
   ;; TODO: it's also: wrap [dip] join dip
   
   'wrap {::spec (stack-spec :a ::item)
          ::fn (stack-effect [a] [[a]])
          ::examples '[[[1 wrap] [[1]]]]}
   ;; TODO: possible security issue with unwrap and bare words on the
   ;; stack: It's possible for malicious code to squat on a word that
   ;; was intended to be data, and not an action word, causing
   ;; unexpected behavior. May want to reconsider whether undefined
   ;; words should be placed onto the stack unquoted.
   'unwrap {::spec (stack-spec :l ::list)
            ::fn (stack-effect [l] l)
            ::examples '[[[[1] unwrap] [1]]]}
   'inscribe {::spec (stack-spec :word ::word
                                 :definition ::program)
              ::fn (fn [{[word word-def & others] ::stack
                         ::keys [expression dictionary]}]
                     {:pre [(-> word name (.startsWith ".") not)]}
                     {::stack others
                      ::expression expression
                      ::dictionary (assoc dictionary word {::definition word-def})})
              ::examples '[[[[3 +] [add3] unwrap inscribe 5 add3] [8]]]}
   'describe {::spec (stack-spec :word ::word)
              ::fn (fn [{[word & others] ::stack dict ::dictionary :as env}]
                     (let [dfn (-> dict (get word) ::definition)]
                       (if dfn
                         (assoc env ::stack (conj others dfn))
                         nothing)))}
   'branch {::spec (stack-spec :false-branch ::program
                               :true-branch ::program
                               :condition ::item)
            ::fn (effect [f t b]
                         [(if (nothing->nil b) t f) []])
            ::examples '[[[5 true [3 *] [4 +] branch] [15]]
                         [[6 false [3 *] [4 +] branch] [10]]]}
   'step {::spec (stack-spec :p ::program
                             :a ::list)
          ::fn (effect [p [agg-item & agg-rest :as agg]]
                       (if (seq agg)
                         [(cond-> ['execute]
                            (seq agg-rest) (concat [(vec agg-rest) p 'step]))
                          [p agg-item]]
                         [[] []]))
          ::examples '[[[1 [2 3 4] [*] step] [24]]
                       [[1 [] [*] step] [1]]]}
   'recur {::spec (stack-spec :rec2 ::program
                              :rec1 ::program
                              :true-branch ::program
                              :false-branch ::program)
           ::fn (effect [rec2 rec1 then pred]
                        ['[if]
                         [(vec (concat rec1 [[pred then rec1 rec2 'recur]] rec2))
                          then pred]])
           ::examples '[[[3 [clone 1 <=] [] [clone dec] [execute *] recur] [6]]]}
   'loop {::spec (stack-spec :p ::program
                             :flag ::item)
          ::fn (effect [p f]
                       [(when (nothing->nil f)
                          (concat p [p 'loop]))
                        []])
          ::examples '[[[10 true [-2 * clone 50 <] loop] [160]]]}
   'pack {::spec (stack-spec :x ::item
                             :l ::list)
          ::fn (stack-effect [x l] [(conj (vec l) x)])
          ::examples '[[[[] 1 pack] [[1]]]
                       [[[1 2 3] 4 pack] [[1 2 3 4]]]]}
   'unpack {::spec (stack-spec :l ::list)
            ::fn (stack-effect [[l & others]] [(nil->nothing l) (vec others)])
            ::examples '[[[["a" "b" "c"] unpack] ["a" ["b" "c"]]]]}
   'join {::spec (stack-spec :l ::list
                             :m ::list)
          ::fn (f-stack 2 (comp vec concat))
          ::examples '[[[["a" "b"] ["c" "d"] join] [["a" "b" "c" "d"]]]]}
   'range {::spec (stack-spec :from ::integer
                              :to ::integer)
           ::fn (f-stack 2 (comp vec range))
           ::examples '[[[1 5 range] [[1 2 3 4]]]]}
   'evert {::spec (stack-spec :l ::list)
           ::doc "Turns the list on top of the stack inside out (puts
                  the list as the rest of the stack, and vice versa)"
           ::fn (fn [{[l & others] ::stack ::keys [dictionary expression]}]
                  {::stack (apply list (vec others) l)
                   ::expression expression
                   ::dictionary dictionary})
           ::examples '[[[1 2 3 [4 5 6] evert] [[3 2 1] 4 5 6]]]}
   'some? {::spec (stack-spec :a ::item)
           ::fn (f-stack 1 (comp some? nothing->nil))}
   'every? {::spec (stack-spec :p ::program
                               :l ::list)
            ::fn (fn [env]
                   #_(update-stack (fn [[p a & others :as stack]]
                                     (->> a
                                          (every? #(leaves-true? (with-stack env (conj others %)) p))
                                          (conj others)))
                                   env))}
   'and {::spec (stack-spec :a ::item
                            :b ::item)
         ::fn (f-stack 2 (fn [a b]
                           (and (nothing->nil a)
                                (nothing->nil b))))
         ::examples '[[[1 odd? 2 even? and] [true]]]}
   'not {::spec (stack-spec :a ::item)
         ::fn (f-stack 1 (fn [a]
                           (not (nothing->nil a))))}
   'intersection {::spec (stack-spec :l ::list
                                     :m ::list)
                  ::fn (f-stack 2 (fn [x y]
                                    (into []
                                          (clojure.set/intersection
                                           (into #{} x)
                                           (into #{} y)))))}})
(swap! core-words merge
       axiom-words
       arithmetic-words
       predicates)

(defn default-env
  ([expression]
   {::stack '()
    ::dictionary @core-words
    ::expression expression})
  ([]
   (default-env '())))

(defn k
  "Run a program with the default env and return the result. Option to
  stop execution after `max-steps` if still unfinished, to prevent
  accidental infinite loops (for debugging purposes)"
  ([opts p]
   (::stack (eval opts (assoc (default-env) ::expression p))))
  ([p] (k {:before-step print-env} p)))

(in-ns 'kcats.core)
(require '[clojure.test :as test])
(defn test
  "Run through all the examples in the default env and make sure they
  work. Takes optional list of words to test"
  ([words]
   (doseq [[word {::keys [examples]}] (if words
                                        (select-keys @core-words words)
                                        @core-words)]
     (test/testing (str word)
       (doseq [[program exp-stack] examples]
         (->> program
              (k {:max-steps 500})
              vec
              (= exp-stack)
              test/is)))))
  ([] (test nil)))

;;TODO: Allow these words to be defined in a .kcats file and read in
;; natively. it would probably mean we can't use spec. But we could
;; allow program defs to include examples and other metadata. Might also
;; be nice to have unit testing be implemented in the language itself?
(in-ns 'kcats.core)
(def standard-words
  {'if {::spec (stack-spec :false-branch ::program
                           :true-branch ::program
                           :condition ::program)
        ::definition '[[execute] dipdown branch]
        ::examples '[[[5 [clone 5 =] [3 *] [4 +] if] [15]]
                     [[6 [clone 5 =] [3 *] [4 +] if] [10]]]}
   'dipdown {::spec (stack-spec :p ::program
                                :a ::item
                                :b ::item)
             ::definition '[wrap [dip] join dip]
             ::examples '[[[1 2 3 [inc] dipdown] [3 2 2]]]}
   'primrec {::spec (stack-spec :rec1 ::program
                                :exit ::program
                                :data ::number)
             ::definition '[[execute] swap join ;; add execute to rec1 to be recur's rec2
                            [[discard] swap join] dip ;; add discard to exit condition
                            [[clone zero?]] dipdown  ;; put the condition on bottom
                            [[clone dec]] dip ;; add the r1
                            recur] ;; now it's generic recur
             ::examples '[[[5 [1] [*] primrec] [120]]]}
   '= {::spec (stack-spec :x ::item, :y ::item)
       ::definition '[..=]
       ::examples '[[[1 1 =] [true]]
                    [["hi" "hi" =] [true]]
                    [["hi" "there" =] [false]]
                    [[[] false =] [false]]
                    [[1 "hi" "hi" =] [true 1]]]}
   'count {::spec (stack-spec :l ::list)
           ::definition '[.count]
           ::examples '[[[["a" "b" "c"] count] [3]]]}
   'prepend {::spec (stack-spec :a ::item
                                :l ::list)
             ::definition '[wrap swap join]
             ::examples '[[[[1 2] 3 prepend] [[3 1 2]]]]}
   'inject {::spec (stack-spec :p ::program
                               :l ::list)
            ::doc "Inject the quoted program into the list below
                   it (runs the program with the list as its
                   stack).  Does not affect the rest of the stack."
            ::definition '[swap evert unpack dip evert]
            ::examples '[[[1 2 3 [4 5 6] [* +] inject] [[26] 3 2 1]]]}
   'snapshot {::spec (stack-spec)
              ::doc "Save the whole stack as a list on the stack"
              ::definition '[[] evert clone evert unwrap]
              ::examples '[[[1 2 3 snapshot] [[3 2 1] 3 2 1]]
                           [[snapshot] [[]]]]}
   'nullary {::spec (stack-spec :p ::program)
             ::doc "Runs program keeping top of stack produced but
                    protects existing items from being consumed."
             ::definition '[[snapshot] dip inject first]
             ::examples '[[[1 2 3 [=] nullary] [false 3 2 1]]]}
   'unary {::spec (stack-spec :p ::program)
           ::definition '[nullary swap discard]}
   'first {::spec (stack-spec :l ::list)
           ::definition '[.first]
           ::examples '[[[[4 5 6] first] [4]]]}
   'second {::spec (stack-spec :l ::list)
            ::definition '[.second]
            ::examples '[[[[4 5 6] second] [5]]]}
   'bytes? {::spec (stack-spec :a ::item)
            ::definition '[.bytes?]}
   'string? {::spec (stack-spec :a ::item)
             ::definition '[.string?]}
   'getbytes {::spec (stack-spec :string ::string)
              ::definition '[.+getBytes]}
   'bytes {::spec (stack-spec :a ::item)
           ::definition '[[[[clone string?] [getbytes]]
                           [[clone bytes?] []]]
                          decide]}
   'map {::spec (stack-spec :p ::program
                            :l ::list)
         ::definition '[[snapshot [] swap] ;; save original stack, and
                        ;; add an empty list to
                        ;; hold results
                        dipdown ;; do this underneath the program and list
                        [[clone] dip wrap] ;; program snippet a to
                        ;; copy the original stack
                        ;; that we saved, will make
                        ;; new copy for each item
                        ;; in the list

                        swap pack ;; pack the map program into the
                        ;; partial program a above

                        ;; inject the map program into the stack copy,
                        ;; take the first item and pack it into the result list
                        [join inject first swap [pack] dip] 
                        join ;; add the program snippet b above to the
                        ;; snippet a, to get a program for 'step'
                        step ;; step through the above program, using
                        ;; the list as data
                        discard ;; we don't need the copy of the
                        ;; original stack anymore
                        ]
         ::examples '[[[[1 2 3] [inc] map] [[2 3 4]]]
                      [[1 [1 2 3] [+] map] [[2 3 4] 1]]
                      [[7 9 [1 2 3] [+ *] map] [[70 77 84] 9 7]]]}
   'nest {::spec (stack-spec :p ::program)
          ::definition '[[] swap
                         ;; wrap the program if it's not []
                         [[[clone [] =] [] [wrap] if] dip join] step]}
   'compose {::spec (stack-spec :composite ::program
                                :p ::program)
             ::doc "Runs program modified by composite by nesting the composite"
             ::definition '[[wrap] dip join nest execute]}
   'map2 {::spec (stack-spec :p ::program
                             :l ::list)
          ::definition '[]
          ::examples '[[[[1 2 3] [inc] map] [[2 3 4]]]
                       [[1 [1 2 3] [+] map] [[2 3 4] 1]]
                       [[7 9 [1 2 3] [+ *] map] [[70 77 84] 9 7]]]}
   'filter {::spec (stack-spec :p ::program
                               :l ::list)
            ::definition '[[snapshot [] swap]
                           dipdown
                           ;; clone the original value so we can save it in results if needed
                           [[clone] dip clone wrap swapdown]
                           swap pack
                           [join inject first 
                            ;; if passes filter, pack it into results
                            [[pack]]
                            ;; othewise discard it
                            [[discard]]
                            branch
                            swapdown
                            dip]
                           join step discard]}})

(swap! core-words merge standard-words)

(in-ns 'kcats.core)

(expound/def ::pair (spec/coll-of ::item :kind vector? :count 2))

(expound/def ::association-list (spec/coll-of ::pair
                                              :kind vector?))
;;TODO: axiom words should be separated
(def associative-words
  {'assign {::spec (stack-spec :item ::pair,
                               :alist ::association-list)
            ::fn (f-stack 2 (fn [alist [k v :as item]]
                              (let [i (.indexOf (mapv first alist) k)]
                                (if (= -1 i)
                                  (conj alist item)
                                  (assoc alist i item)))))
            ::examples '[[[[[a b] [c d]] [a x] assign] #_-> [[[a x] [c d]]]]
                         [[[[a b] [c d]] [e x] assign] #_-> [[[a b] [c d] [e x]]]]]}
   'lookup {::spec (stack-spec :key ::item
                               :map ::association-list)
            ::fn (f-stack 2 (fn [alist key]
                              (get (into {} alist) key [])))
            ::examples '[[[[[a b] [c d]] a lookup] [b]]
                         [[[[a b] [c d]] e lookup] [[]]]]}
   'decide {::spec (stack-spec :test-expr-pairs ::association-list
                               :other (spec/* ::item))
            ::doc "Takes a list of choices (pairs of test, program) and
            executes the first program whose test passes. if none
            pass, returns 'nothing'. Stack is reset between
            testing conditions."
            ::fn (fn [{[[[test expr :as first-clause]
                         & other-clauses]
                        & others] ::stack
                       ::keys [dictionary expression]}]
                   {::expression (if first-clause
                                   (concat [[test 'nullary] ;; run test resetting stack
                                            expr ;; the then
                                            [(vec other-clauses) 'decide] ;; the else
                                            'if]
                                           expression)
                                   expression)
                    ::stack (cond-> others
                              ;; if conditions are empty result is empty list
                              (not first-clause) (conj []))
                    ::dictionary dictionary})
            ::examples '[[[5 [[[3 =] ["three"]]
                              [[5 =] ["five"]]
                              [[7 =] ["seven"]]
                              [[true] ["something else"]]]
                           decide]
                          ["five" 5]]
                         [[9 [[[3 =] ["three"]]
                              [[5 =] ["five"]]
                              [[7 =] ["seven"]]
                              [[true] ["something else"]]]
                           decide]
                          ["something else" 9]]
                         [[9 [[[3 =] ["three"]]
                              [[5 =] ["five"]]
                              [[7 =] ["seven"]]]
                           decide]
                          [[] 9]]]}
   'type {::spec (stack-spec :alist ::association-list)
          ::definition '[[clone count 1 =] ;; if it's a single item
                         [first first] ;; the type is the key of that first item
                         [[type] unwrap lookup] ;; otherwise look up the key 'type'
                         if]
          ::examples '[[[[[foo 1]] type] [foo]]
                       [[[[type url] [value "http://foo.com"]] type] [url]]]}
   'value {::spec (stack-spec :alist ::association-list)
           ::definition '[[clone count 1 =]
                          [first second]
                          [[value] unwrap lookup]
                          if]
           ::examples '[[[[[foo 1]] value] [1]]
                        
                        [[[[type url] [value "http://foo.com"]] value]
                         ["http://foo.com"]]]}})

(swap! core-words merge associative-words)

(swap! core-words merge
       {'addmethod {::spec (stack-spec :condition ::pair
                                       :word ::word)
                    ::definition '[[clone describe] dip ;; get definition of word beneath
                                        ; and keep an extra copy for later inscribe
                                   pack ;; new condition onto end of definition
                                   [rotate ;; new condition to 1st
                                           ;; position - TODO: note this
                                           ;; could fail if the def is
                                           ;; more than just an alist
                                           ;; and 'cond'.
                                    pack] ;; new condition onto end of conditions
                                   inject ;; the above program to run on the conditions
                                   swap inscribe ;; redefine word
                                   ]}})

(in-ns 'kcats.core)
(declare env->clj)

(expound/def ::nested-environment
  (spec/and ::association-list #(->> % env->clj (spec/valid? ::environment))))

(in-ns 'kcats.core)

(defn env->clj
  "Convert an env from kcats format to clj"
  [e]
  (let [ec (into {} e)]
    {::stack (-> ec (get 'stack) list* (or '()))
     ::dictionary (merge core-words (into {}
                                          (for [[k v] (get ec 'dictionary)]
                                            [k {::definition v}])))
     ::expression (or (list* (get ec 'expression)) '())}))

(defn env->k
  "Convert an env from clj format to kcats"
  [e]
  [['stack (-> e ::stack vec)]
   ['dictionary (into {}
                      (for [[k {::keys [definition]}] (::dictionary e)
                            :when definition]
                        [k definition]))]
   ['expression (-> e ::expression vec)]])

(in-ns 'kcats.core)
(swap! core-words merge
       {'eval-step {::spec (stack-spec :environment ::environment)
                    ::fn (f-stack 1 (comp env->k eval-step env->clj))}
        'spawn {::spec (stack-spec :expression ::list)
                ::fn (fn [{::keys [stack] :as env}]
                       ;;(print-env env)
                       (let [expr (first stack)]
                         (->> expr
                              default-env
                              (eval {:before-step print-env})
                              future))
                       (update env ::stack rest))}})
