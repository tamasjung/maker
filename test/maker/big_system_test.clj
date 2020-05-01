(ns maker.big-system-test
  (:require [clojure.test :refer :all]
            [maker.core :as m :refer :all]))


;-------------------------------------------------------------------------------

(def n 200)                                                 ;700 is still good but too slow as part of test suite
(def max-number-of-params 20) ;you can't specify more than 20 params in clojure
(def id "asdf")                                             ;you can play with the length of the symbols
(def symbols (mapv #(hash-map :sym (symbol (str id %))
                              :n %)
                   (range n)))

(eval
  `(do
     ~@(for [i (reductions conj [] symbols)
             :when (seq i)
             :let [params (mapv :sym (->> i butlast (take-last max-number-of-params)))]]
         `(defgoal ~(-> i last :sym)
                   ~params
                   (+' ~@params ~(->> i last :n))))))

(deftest static-test
  ;watch with macroexpand the huge let
  (is (= (make asdf199)
         803401211439151610573502084713732901886520312187133440346102N)))

(eval
  `(deftest big-fn
     (is (= (make ~(->> symbols last :sym))
            (->> n
                 range
                 (reduce
                   (fn [nums# i#]
                     (let [last-20# (vec (take-last max-number-of-params nums#))]
                       (conj last-20# (reduce +' i# last-20#))))
                   [])
                 last)))))

