(ns maker.core-test
  (:require [clojure.test :refer :all]
            [maker.core :as m :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [ns2 :refer [ns2a*]]))

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
  (is (thrown? Throwable
               (make goal-with-dyn-dep))))

;-------------------------------------------------------------------------------

(def call-counter (atom 0))

(defn factor*
  []
  (swap! call-counter inc)
  2)

(defn iterator-items*
  []
  (range 10))

(defn collected-item*
  [factor iterator-item]
  (* iterator-item factor))

(defrelation collected-items* :for iterator-items)

(deftest test-static-collectors
  (is (= (last (make collected-items))
         18))
  ;; despite it is required inside the iteration
  ;; 'factor' is made in the 'right' place and called only once
  (is (= @call-counter 1)))

(declare ^{:for '[iterator-item iterator-items]
           :collect 'collected-item}
         another-collected-items*)

(deftest test-for-vector
  (is (= (last (make another-collected-items))
         18)))

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

(defn self* [self]
  1)

(deftest circular-dep
  (is (thrown? Throwable (eval '(make self)))))

;-------------------------------------------------------------------------------

(defn multi-a*
  []
  'a)

(defn bb*
  []
  'bb)

(defn multi-b*
  [bb]
  'b)

(defn my-selector*
  [d]
  (case (:type d)
    :a 'multi-a
    :b 'multi-b))

(declare ^{:selector 'my-selector
           :cases '[multi-a multi-b]}
         multigoal*)

(deftest test-multi-deps
  (let [d {:type :a}]
    (is (= 'a (make multigoal))))
  (let [d {:type :b}]
    (is (= 'b (make multigoal)))))

;-------------------------------------------------------------------------------

(defn not-common*
  [])

(defn common-g*
  [])

(defn common-it-g*
  [m-it])

(defn m-its*
  []
  [{:type 'm-aa} {:type 'm-bs}])

(defn m-sel*
  [m-it]
  (:type m-it))

(defrelation m :selector m-sel :cases [m-aa m-bs])

(defn m-subits* [])

(defn m-sub*
  [m-subit])

(defrelation m-subs* :for m-subits)

(defn m-a*
  [m-it common-g common-it-g not-common m-subit]
  (assoc m-it :m :a))

(defn m-b*
  [m-it common-g common-it-g m-subit]
  (assoc m-it :m :b))

(defrelation m-as* :for m-subits)

(defrelation m-bs* :for m-subits)

(defn m-aa* [m-as])

(declare ^{:for 'm-its} ms*)

(deftest test-iterative-multi
  (is (= (->> (make ms)
              count)
         2)))

;-------------------------------------------------------------------------------

(defn model-ones*
  []
  [[1 2] [3 4]])

(defn model-twos*
  [model-one]
  model-one)

(defn view-two*
  [model-two]
  (str model-two))

(declare ^{:for 'model-twos} view-twos*)

(defn view-one*
  [view-twos]
  (string/join "-" view-twos))

(declare ^{:for '[model-one model-ones]
           :collect 'view-one} view-ones*)

(deftest two-levels-iteration-with-metas
  (is (= (make view-ones)
         (list "1-2"
               "3-4"))))
;; or in shorter way
(defrelation view-ones*
  :for model-ones)

(deftest two-levels-iteration-with-defrel
  (is (= (make view-ones)
         (list "1-2"
               "3-4"))))