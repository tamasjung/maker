(ns maker.core-test
  (:require [clojure.test :refer :all]
            [maker.core :as m :refer :all]
            [clojure.pprint :refer :all]
            [ns2 :as ns-two]))

(deftest munge-test
  (are [s] (-> s inj-munge inj-munge-inv (= s))
    "aa"
    "a.b/c"
    "ab/c"))

#_(deftest peek-conj-test
  (is (= (conj-top [#{}] 'a)
         [#{'a}])))

(defn d* [] 5)
(defn three-times* [d] (* 3 d))
(defn six-times* [three-times] (* 2 three-times))

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

(declare dd*)

(defn goal-with-dyn-dep* [dd] (+ 10 dd))

(deftest test-dynamic-goal
  (is (= (let [dd 1]
           (make goal-with-dyn-dep))
         11))
  (is (thrown? Throwable
               (make goal-with-dyn-dep))))

(def call-counter (atom 0))

(defn item-factor*
  []
  (swap! call-counter inc)
  2)

(defn generator-items*
  [item-factor]
  (range 10))

(declare ^{:for 'generator-items} generator-item*)

(defn rel-item*
  [generator-item item-factor]
  (* generator-item item-factor))

(declare ^{:collect 'rel-item} rel-items*)

(deftest test-collectors
  (is (= (last (make rel-items))
         18)))

(defn generator-items2*
  [item-factor]
  ["a" "b"])

(declare ^{:for 'generator-items2} generator-item2*)

(defn pair*
  [item-factor generator-item generator-item2]
  [generator-item generator-item2])

(def ^{:collect 'pair} pairs*)

#_(defn pairs*
  [generator-items generator-items2 item-factor]
  (for [generator-item generator-items
        generator-item2 generator-items2]
    (make pair)))

(deftest test-comb
  (reset! call-counter 0)
  (is (= (count (prn-make pairs))
         20))
  (is (= @call-counter 1)))

(defn m* [] {:a 1 :b 2})
(defn v* [] [11 22])

(defn destr-goal*
  [{:keys [a b] :as m}  [c :as v]]
  (list a b m c v))

(deftest test-destr
  (is (= (make destr-goal)
         (list 1 2 {:a 1 :b 2} 11 [11 22]))))

(declare dm*)
(declare dv*)

(defn d-destr-goal*
  [{:keys [a b] :as dm} [c :as dv]]
  (list a b dm c dv))

(deftest test-d-destr
  (is (= (let [dm {:a 1 :b 2}
               dv [11 22]]
           (prn-make d-destr-goal))
         (list 1 2 {:a 1 :b 2} 11 [11 22]))))
