(ns maker.core-test
  (:require [clojure.test :refer :all]
            [maker.core :as m :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [ns2 :refer [ns2a*]]
    #_[clojure.core.async :as a]))

;-------------------------------------------------------------------------------

(deftest munge-test
  (are [s res] (-> s inj-munge (= res))
               "aa" "aa"
               "a.b/c" "a_b!c"
               "ab/c" "ab!c"))

;-------------------------------------------------------------------------------

(defn d* [] 5)

;; d is called a 'goal'
;; d* is the 'maker function

(deftest simple-goal
  (is (= 5 (make d))))

(defn three-times* [d] (* 3 d))

;; three-times depends on d above

(deftest simple-dependency
  (is (= (make three-times)
         15)))

(defn six-times* [three-times] (* 2 three-times))

;; six-times depends transitively on d

(deftest base-transitive-dep-test
  (is (= (make six-times)
         30))

  ;; shadow the original definition simply with let
  (is (= (let [d 10]
           (make six-times))
         60)))

;-------------------------------------------------------------------------------

(deftest test-across-nss
  ;; for external namespaces maker munges the binding names,
  ;; check it with macroexpansion
  (is (= 222 (make ns2a))))

;-------------------------------------------------------------------------------

(defn goal-with-dyn-dep* [dd] (+ 10 dd))

(deftest test-dynamic-goal
  (is (= (let [dd 1]
           (make goal-with-dyn-dep))
         11))
  ;FIXME another ex
  (is (thrown? Throwable
               (eval '(make goal-with-dyn-dep)))))

;-------------------------------------------------------------------------------

(def call-counter (atom 0))

(defn factor*
  []
  (swap! call-counter inc)
  2)

(defgoal? iterator-item)

(defn iterator-items*
  []
  (range 10))

(defn collected-item*
  [factor iterator-item]
  (* iterator-item factor))

(defgoal! collected-items
  [iterator-items factor]
  (map (fn [iterator-item]
         (make collected-item))
       iterator-items))

(deftest test-static-collectors
  (is (= (last (make collected-items))
         18))
  ;; despite it is required inside the iteration
  ;; 'factor' is made in the 'right' place and called only once
  (is (= @call-counter 1)))

(defgoal! another-collected-items
  [iterator-items]
  (map (fn [iterator-item]
         (* 2 (make collected-item)))
       iterator-items))

(deftest test-for-vector
  (is (= (last (make another-collected-items))
         36)))

;-------------------------------------------------------------------------------

(defn m* [] {:a 1 :b 2})
(defn v* [] [11 22])

;; we work together with destructuring
(defn destr-goal*
  [{:keys [a b] :as m} [c :as v]]
  (list a b m c v))

(deftest test-destr
  (is (= (make destr-goal)
         (list 1 2 {:a 1 :b 2} 11 [11 22]))))

;; destructuring wiht dynamic goals

(defn d-destr-goal*
  [{:keys [a b] :as dm} [c :as dv]]
  (list a b dm c dv))

(deftest test-d-destr
  (is (= (let [dm {:a 111 :b 222}
               dv [1 2]]
           (make d-destr-goal))
         (list 111 222 {:a 111 :b 222} 1 [1 2]))))

;-------------------------------------------------------------------------------

;; circular dependency is an error at compile time

(defn self*
  [self])

(deftest circular-dep
  ;FIXME false test method
  (is (thrown? Throwable (eval '(make self)))))

;-------------------------------------------------------------------------------

(defgoal model-ones
  []
  [[1 2] [3 4]])

(defgoal model-twos
  [model-one]
  model-one)

(defgoal? model-two)
(def model-one*)

(defn view-two*
  [model-two]
  (str model-two))

(defgoal! view-twos
  [model-twos]
  (for [model-two model-twos]
    (make view-two)))

(defgoal view-one
  [view-twos]
  (string/join "-" view-twos))

(defgoal! view-ones
  [model-ones]
  (map (fn [model-one] (*- view-one))
       model-ones))

(deftest two-levels-iteration-with-metas
  (is (= (make view-ones)
         (list "1-2"
               "3-4"))))

(defgoal! view-ones
            [model-ones]
            (map (fn [model-one] (*- view-one)) model-ones))

(deftest two-levels-iteration-with-defrel
  (is (= (make view-ones)
         (list "1-2"
               "3-4"))))