(ns maker.core-test
  (:require [clojure.test :refer :all]
            [maker.core :as m :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [ns2 :refer [ns2a*]]))

(deftest munge-test
  (are [s res] (-> s inj-munge (= res))
    "aa" "aa"
    "a.b/c" "a_b!c"
    "ab/c" "ab!c"))

#_(deftest peek-conj-test
  (is (= (conj-top [#{}] 'a)
         [#{'a}])))

(defn d* [] 5)
(defn three-times* [d] (* 3 d))
(defn six-times* [three-times] (* 2 three-times))

(deftest test-deps
  (pprint (make six-times)))

(defn make-caller
  [d]
  ;; will print (let [] d)
  (make d #_(pprint %&)))

(deftest inserted-test
  (is (= (make six-times)
         30))
  (is (= (let [d 10]
           (make six-times))
         60))
  (is (= (let [d 10
               d 20]
           (make six-times))
         120))
  (is (= (make-caller 3)
         3)))

(deftest test-different-ns
  (is (= 222 (*- ns2a))))
;
(declare dd*)

(defn goal-with-dyn-dep* [dd] (+ 10 dd))

(deftest test-dynamic-goal
  (is (= (let [dd 1]
           (make goal-with-dyn-dep))
         11))
  (is (thrown? Throwable
               (make goal-with-dyn-dep))))

(def call-counter (atom 0))

(defn factor*
  []
  (swap! call-counter inc)
  2)

(defn iterator-items*
  []
  (range 10))

(declare iterator-item*)                                    ;;FIXME from macro

(defn collected-item*
  [factor {:keys [] :as iterator-item}]
  (* iterator-item factor))

;FIXME would not work without iterator-item decl above, macro?
(declare ^{:for 'iterator-items
           :item 'iterator-item
           :collect 'collected-item}
         collected-items*)

(deftest test-static-collectors
  (let [facsdftor 2]
    (is (= (last (make collected-items))
           18))))

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
           (make d-destr-goal))
         (list 1 2 {:a 1 :b 2} 11 [11 22]))))

(defn self* [self]
  self)

(deftest circular-dep
  (is (thrown? Throwable (eval '(make self)))))

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
           :cases ['multi-a 'multi-b]}
         multigoal*)

(deftest test-multi-deps
  (let [d {:type :a}]
    (is (= 'a (*- multigoal))))
  (let [d {:type :b}]
    (is (= 'b (*- multigoal)))))

;;-------------------------------------------------------------------------------
;
;(defn not-common*
;  [])
;
;(defn common-g*
;  [])
;
;(defn common-it-g*
;  [m-it])
;
;(defn m-its*
;  [not-common]
;  [{:type 'm-a} {:type 'm-b}])
;
;(declare ^{:for 'm-its} m-it*)
;
;(defn m-sel*
;  [m-it]
;  (:type m-it))
;
;(declare ^{:selector 'm-sel :cases ['m-a 'm-b]} m*)
;
;#_(defcase m-sel* [m-a m-b])
;
;#_(defcoll ms* :from m-its)
;
;(defn m-a*
;  [m-it common-g common-it-g not-common]
;  (assoc m-it :m :a))
;
;(defn m-b*
;  [m-it common-g common-it-g]
;  (assoc m-it :m :b))
;
;(declare ^{:collect 'm} ms*)
;
;(deftest test-iterative-multi
;  (is (= (->> (*- ms)
;              (map :m))
;         [:a :b])))
;
;(defn model-1s*
;  []
;  [[1 2] [3 4]])
;
;(def ^{:for 'model-1s} model-1*)
;
;(defn model-2s*
;  [model-1]
;  model-1)
;
;(def ^{:for 'model-2s} model-2*)
;
;(defn view-2*
;  [model-2]
;  (str model-2))
;
;(def ^{:collect 'view-2} view-2s*)
;
;(defn view-1*
;  [view-2s]
;  (string/join "-" view-2s))

