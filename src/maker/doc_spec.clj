(ns maker.doc-spec
  (:require [maker.core :as m]
            [clojure.spec.alpha :as s]
            [clojure.future :refer :all]))

(s/def ::goal-var var?)
(s/def ::goal-local symbol?)
(s/def ::goal-meta map?)
(s/def ::goal-map (s/keys :req-un [::goal-local
                                   ::goal-var
                                   ::goal-meta]))
(s/def ::ready-bits int?)
(s/def ::dep-index (s/map-of ::goal-map int?))
(s/def ::dep-values vector?)
(s/def ::async-state (s/keys :req-un [::ready-bits
                                      ::dep-index
                                      ::dep-values]))
(s/def ::maker-call any?)
(s/def ::binding-pairs (s/+ (s/cat :goal-local ::goal-local
                                   :call ::maker-call)))
(s/def ::bindings (s/map-of ::goal-map ::binding-pairs))
(s/def ::rev-dep-goals (s/map-of ::goal-map (s/coll-of ::goal-map)))
(s/def ::goal-maps (s/coll-of ::goal-map))
(s/def ::walk-goal-list ::goal-maps)
(s/def ::local-env (s/coll-of ::goal-local :into #{}))
(s/def ::maker-state (s/keys :req-un [::bindings
                                      ::rev-dep-goals
                                      ::walk-goal-list
                                      ::local-env]))
(s/def ::ns any?)
(s/def ::graph ::maker-state)
(s/def ::starters ::goal-maps)
(s/def ::used-from-env (s/map-of ::goal-local ::goal-map))
(s/def ::results (s/map-of ::goal-map any?))
(s/def ::result (s/tuple string? #_boolean? any?))
(s/def ::async-ctx (s/keys :req-un [::ns
                                    ::graph
                                    ::starters
                                    ::used-from-env
                                    ::goal-map
                                    ::results
                                    ::result]))

(s/fdef m/has-result
        :args (s/cat :ctx ::async-ctx :goal ::goal-map)
        :ret boolean?)

(s/fdef m/make-internal
        :args (s/cat :state ::maker-state :goal ::goal-map)
        :ret any?)
