(ns ns2
  (:require [ns1 :refer [ns1a*]]
            [ns3 :refer [ns3a*]]))

(defn ns2a*
  [ns1a]
  (* 2 ns1a))

(defn ns3a-proxy*
  [ns3a]
  ns3a)

(defn ns2c*
  []
  22)

(declare ns2i*)

(defn ns2b*
  [ns2c ns2i]
  (str ns2c ns2i))
