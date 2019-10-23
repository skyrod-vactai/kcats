(ns kcats.core
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound])
  (:refer-clojure :exclude [eval]))

(set! s/*explain-out* (expound/custom-printer
                       {:show-valid-values? true}))
(s/check-asserts true)

(s/def ::boolean boolean?)
(s/def ::number number?)
(s/def ::string string?)
(s/def ::bytes bytes?)

(expound/def ::item (s/or :aggregate ::aggregate
                          :boolean ::boolean
                          :number ::number
                          :string ::string
                          :bytes ::bytes
                          :word ::word))

(expound/def ::aggregate (s/coll-of ::item :kind vector?) "aggregate?")
(expound/def ::program ::aggregate)

(expound/def ::word symbol? "word?")

(s/def ::dictionary (s/map-of ::word ::program))

(s/def ::stack (s/coll-of ::stack-item
                          :kind list?))

(s/def ::environment (s/keys :req [::stack ::dictionary]))
