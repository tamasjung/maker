(ns maker.spec
  (:require [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]
            [clojure.future :refer :all]
            [maker.core :as m]))

(defn goal-map-spec
  [goal-map]
  (let [sp (-> goal-map
               m/goal-maker-symbol
               s/get-spec)]
    (or (:ret sp)
        sp
        any?)))

(defmacro def
  [goal-name spec]
  `(s/def ~(->> goal-name
                (m/goal-param-goal-map *ns*)
                m/goal-maker-symbol)
     ~spec))

(defmacro fdef
  "Infer args' spec from dependencies' :ret"
  [goal-name ret-spec & opts]
  `(s/fdef ~(->> goal-name
                 (m/goal-param-goal-map *ns*)
                 m/goal-maker-symbol)
           ~@(->> opts
                  (merge {:ret ret-spec
                          :args `(s/cat ~@(->> goal-name
                                               (m/goal-param-goal-map *ns*)
                                               m/goal-map-dep-goal-maps
                                               (mapcat (juxt (comp keyword
                                                                   :goal-local)
                                                             goal-map-spec))))})
                  (reduce concat []))))

