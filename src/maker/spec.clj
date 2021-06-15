(ns maker.spec
  (:require [clojure.spec.alpha :as s]
            [maker.core :as m]))

(defn- goal-map-spec
  [goal-var]
  (let [sp (-> goal-var
               m/goal-maker-symbol
               s/get-spec)]
    (or (:ret sp)
        sp
        any?)))

(defmacro def
  [goal-name spec]
  `(s/def ~(->> goal-name
                (m/goal-sym-goal-var *ns*)
                m/goal-maker-symbol)
     ~spec))

(defmacro fdef
  "Infer args' spec from dependencies' :ret"
  [goal-name ret-spec & opts]
  `(s/fdef ~(->> goal-name
                 (m/goal-sym-goal-var *ns*)
                 m/goal-maker-symbol)
           ~@(->> opts
                  (merge {:ret ret-spec
                          :args `(s/cat ~@(->> goal-name
                                               (m/goal-sym-goal-var *ns*)
                                               (#'m/dependencies)
                                               (mapcat (juxt (comp keyword
                                                                   (partial m/goal-var-goal-local *ns*))
                                                             goal-map-spec))))})
                  (reduce concat []))))

