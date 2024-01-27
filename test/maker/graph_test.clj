(ns maker.graph-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [maker.graph :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(defn topsort-plain
  [successor-fn start]
  (->> (top-sorting successor-fn {:stack [start]
                                  :sorted-set #{}
                                  :stack-set #{start}})
       (mapv first)))

(deftest basic-test
  (is (= (topsort-plain {0 [1 2]
                         1 [2]
                         2 []}
                        0)
         [2 1 0])))

(deftest circularity-check
  (is (thrown-with-msg? ExceptionInfo #"Circular"
                        (topsort-plain {0 [1]
                                        1 [0]}
                                       0)))

  (is (thrown-with-msg? ExceptionInfo #"Circular"
                        (topsort-plain {0 [0]}
                                       0)))

  (is (thrown-with-msg? ExceptionInfo #"Circular"
                        (topsort-plain {0 [0]
                                        1 [1]}
                                       0))))

(deftest closest-independents-test
  (let [nodes {:a [:b :c]
               :b [:c :d :e]
               :c [:d]
               :d []
               :e [:f]
               :f nil}]
    (are [root dependent-pred independents]
      (= (second (collect-closest-independents nodes dependent-pred root)) independents)

      :a #{:d} #{:e}
      :a #{:e} #{:c :d}
      :b #{:c} #{:d :e}
      :a #{} #{:a}
      :a #{:a} #{}
      :d #{} #{:d}
      :f #{:f} #{}
      :f #{} #{:f})))


(deftest perf-test

  (let [n 1000
        g (->> (range n)
               (map #(vector % (range %)))
               (into {}))]
    ;(pprint (sort-by first g))
    (doseq [f [
               topsort-plain
               ]]
      (print (class f) ": ")
      (time (print (count (f g (dec n)))))))

  (let [n 10000
        g (->> (range n)
               (map #(vector % [(inc %)]))
               (into {n []}))]
    ;(pprint (sort-by first g))
    (doseq [f [
               topsort-plain
               ]]
      (print (class f) ": ")
      (time (print (count (f g 0)))))))

