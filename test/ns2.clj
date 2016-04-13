(ns ns2
  (:require [ns1 :refer [ns1a*]]))

(defn ns2a*
  [ns1a]
  (* 2 ns1a))
