(ns maker.core-test
  (:require [clojure.test :refer :all]
            [maker.core :as m :refer :all]
            [ns2 :as ns-two]))

(defgoal d [] 5)
(defgoal three-times [[c d]] (* 3 c))
(defgoal twice [three-times] (* 2 three-times))

(deftest inserted-test
  (is (= (make twice)
         30))
  (is (= (with
           [d 10]
           (make twice))
         60))
  (is (= (with
           [d 10
            d 20]
           (make twice))
         120)))

(deftest test-different-ns
  (is (= 55 (make ns-two/b)))
  (is (= (with
           [ns1/a 22]
           (make ns-two/b))
         110)))

(declare-goal dd)

(defgoal goal-with-dyn-dep [dd] (+ 10 dd))

(deftest test-dynamic-goal
  (is (= (with
           [dd 1]
           (make goal-with-dyn-dep))
         11))
  (is (thrown? Throwable
               (make goal-with-dyn-dep))))
