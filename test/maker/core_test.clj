(ns maker.core-test
  (:require [clojure.test :refer :all]
            [maker.core :as m :refer :all]
            [clojure.pprint :refer :all]
            [ns2 :as ns-two]))
(defn log-> [& xs] (apply prn "log->" xs) (first xs))(defn log->> [& xs] (apply prn "log->>" xs) (last xs))

(deftest munge-test
  (are [s] (-> s inj-munge inj-munge-inv (= s))
    "aa"
    "a.b/c"
    "ab/c"))

(deftest peek-conj-test
  (is (= (conj-top [#{}] 'a)
         [#{'a}])))

(defgoal d [] 5)
(defgoal three-times [d] (* 3 d))
(defgoal six-times [three-times] (* 2 three-times))

(deftest inserted-test
  (is (= (make six-times)
         30))
  (is (= (let [d 10]
           (make six-times))
         60))
  (is (= (let [d 10
               d 20]
           (make six-times))
         120)))

#_(deftest test-different-ns
  (is (= 55 (make ns-two/b)))
  (is (= (let [ns1/a 22]
           (make ns-two/b))
         110)))

(declare-goal dd)

(defgoal goal-with-dyn-dep [dd] (+ 10 dd))

(deftest test-dynamic-goal
  (is (= (let
           [dd 1]
           (make goal-with-dyn-dep))
         11))
  (is (thrown? Throwable
               (make goal-with-dyn-dep))))

(defgoal factor
         []
         4)

(defgoal i-s
         []
         (range 2))

(declare-goal i)

;;(defrelation is i)

(defgoal j
         [i factor]
         (* factor i))

(defgoal item
         [j]
         (inc j))

;;v.
(defgoal ^:in-context items
         [i-s]
         (for [i i-s]
           (make item)))

(defgoal sum-of-items
         [items]
         (reduce + 0 items))

(defgoal ^:in-context bigger-sum-of-items
  []
  (let [i-s (range 10)]
        (make sum-of-items)))

(deftest in-context-test
  (is (= (make bigger-sum-of-items)
         190))
  (is (= (make items)
         (list 1 5))))

(defn fa [] 1)
(defn fb [fa] (* 2 fa))
(defn fc [fb] (* 3 fb))
(deftest test-plain-functions
  (is (= (make fc)
         6)))

(defn generator-items
  []
  (range 10))

(declare ^{:for 'generator-items} generator-item)

(defn rel-item
  [generator-item]
  (* generator-item 2))

(declare ^{:relation 'rel-item} rel-items)

(deftest test-relations
  (is (= (last (make rel-items))
         18)))


