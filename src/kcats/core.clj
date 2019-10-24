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

(expound/def ::word symbol? "word?")

(spec/def ::axiom-definition (spec/keys :req [::fn ::spec]))
(spec/def ::program-definition (spec/keys :req [::fn ::definition]))

(spec/def ::dictionary (spec/map-of ::word (s/or ::axiom-definition
                                                 ::program-definition)))

(spec/def ::stack (spec/coll-of ::stack-item
                                :kind list?))

(spec/def ::environment (spec/keys :req [::stack ::dictionary]))

(declare eval-item) ;; we'll define this later

(def eval (partial reduce #'eval-item))

(defmulti eval-item
  "Evaluate one item in the given environment. A single step in a
  program's execution."
  (fn [env item]
    (type item)))

(defn- push [env item]
  (update env :stack conj item))

(defmethod eval-item Number [env item]
  (push env item))

(defmethod eval-item String [env item]
  (push env item))

(defmethod eval-item Boolean [env item]
  (push env item))

;; PersistentVector is the clojure type for an aggregate
(defmethod eval-item clojure.lang.PersistentVector [env item]
  (push env item))

(defmethod eval-item (Class/forName "[B") [env item]
  (push env item))

(defmethod eval-item clojure.lang.Symbol [env word]
  (let [{f :fn spec :spec} (-> env :dict word)]
    (when spec
      (spec/assert spec (:stack env)))
    (if f
      (f env)
      (push env word))))
