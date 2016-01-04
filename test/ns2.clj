(ns ns2
  (:require [maker.core :as g]
            [ns1 :as ns-one]))

(g/defgoal b
           [[c ns-one/a]]
           (* c 5))