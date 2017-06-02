(ns maker.core-test
  (:require [clojure.test :refer :all]
            [maker.core :as m :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [ns2 :refer [ns2a*]]
            [ns1 :refer [ns1a*]]
            [clojure.core.async :as a])
  (:import (clojure.lang ExceptionInfo)))

;-------------------------------------------------------------------------------

(deftest munge-test
  (are [s res] (-> s inj-munge (= res))
               "aa" "aa"
               "a.b/c" "a_b!c"
               "ab/c" "ab!c"))

;-------------------------------------------------------------------------------

(defn simple*
  []
  "simple")

;; simple is called a 'goal'
;; simple* is the 'maker function

(deftest test-simple
  (is (= "simple"
         ;let's make the goal
         (make simple)
         ;the make macro expands to
         (let [simple (simple*)]
           simple))))

(defn other*
  [simple]
  (str simple "-other"))


(defn another*
  [other]
  (str other "-another"))


(deftest base-transitive-dep-test
  (is (= (make another)
         "simple-other-another"
         (let [simple (simple*)
               other (other* simple)
               another (another* other)]
           another)))

  ;; shadow the original definition simply with let
  (is (= (let [simple "changed"]
           (make another))
         "changed-other-another")))

;-------------------------------------------------------------------------------

(deftest test-across-nss
  ;; for external namespaces maker munges the binding names,
  ;; check it with macroexpansion
  (is (= 222 (make ns2a)))
  ;with-goals is an extended let, works well with goal from another namespace.
  (is (= 22 (with-goals [ns1a 11]
              (make ns2a)))))

;-------------------------------------------------------------------------------

(declare dd*)

(defn goal-with-dyn-dep* [dd] (+ 10 dd))

(deftest test-dynamic-goal
  (is (= (let [dd 1]
           (make goal-with-dyn-dep))
         11))
  (is (= (with-goals [dd 2]
           (make goal-with-dyn-dep))
         12)))

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


(defgoal collected-items
  [? iterator-items]
  (map (fn [iterator-item]
         (make collected-item))
       iterator-items))

(deftest test-static-collectors
  (is (= (last (make collected-items))
         18))
  ;; despite it is required inside the iteration
  ;; 'factor' is made in the 'right' place and called only once
  (is (= @call-counter 1)))

(defgoal another-collected-items
  [? iterator-items]
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

(defgoal? dm)
(defgoal? dv)

(deftest test-d-destr
  (is (= (let [dm {:a 111 :b 222}
               dv [1 2]]
           (make d-destr-goal))
         (list 111 222 {:a 111 :b 222} 1 [1 2]))))

;-------------------------------------------------------------------------------

;; circular dependency is an error at compile time


(deftest circular-dep
  ;FIXME false test method
  (is (thrown? Throwable
               (eval '(do
                        (defn self*
                          [self])
                        (make self))))))

;-------------------------------------------------------------------------------


(defgoal model-ones
  []
  [[1 2] [3 4]])

(defgoal model-twos
  [model-one]
  model-one)

(defgoal? model-two)
(declare model-one*)

(defn view-two*
  [model-two]
  (str model-two))

(defgoal view-twos
  [model-twos]
  (for [model-two model-twos]
    (make view-two)))

(defgoal view-one-factor
  [])

(defgoal view-one
  [view-twos view-one-factor]
  (string/join "-" view-twos))

(defgoal view-ones
  [? model-ones]
  (map (fn [model-one] (make view-one))
       model-ones))

(deftest two-levels-iteration
  (is (= (make view-ones)
         (list "1-2"
               "3-4"))))

;-------------------------------------------------------------------------------

(defgoal? n)

(defgoal<> urls
  [n]
  (let [res (a/promise-chan) #_(a/chan 1)]
    (future
      (try
        (->> (range n)
             (map (partial str "url"))
             (a/put! res))
        (catch Throwable th
          (a/put! res th))))
    res))

(defgoal? url)

(defgoal<- content
  [url]
  (future
    (try
      (yield (str url "content"))
      (catch Throwable th
        (yield th)))))

(defgoal<> contents
  [urls]
  (let [res-ch (a/chan (count urls))]
    (a/pipeline-async 10
                      res-ch
                      (fn [url result-ch]
                        (a/go (a/>! result-ch (a/<! (make<> content)))
                              (a/close! result-ch)))
                      (a/to-chan urls))
    (a/into [] res-ch)))

(deftest test-async
  (let [n 10]
    (is (= n
           (-> contents make<> a/<!! count)))))

;-------------------------------------------------------------------------------
;Example support for reloaded framework.

(def release-fns (atom (list)))

(def release-by
  "The goal is a function to store created goal as it happens."
  (partial swap! release-fns conj))

(defn stop-system
  []
  (doseq [f @release-fns]
    (f)
    (swap! release-fns rest)))

;The three things above consist the generic support for a reloaded workflow.
;Find below the sample 'components'.

(defgoal config
  []
  (release-by #(println "Release the config."))
  "the config")

(defgoal db-conn
  [config]
  (release-by #(println "Release the db-conn."))
  (str "the db-conn"))

(deftest my-little-component-framework
  (is (= (make db-conn)
         "the db-conn"))
  (is (= (count @release-fns)
         2))
  (is (= (with-out-str
           (stop-system))
         "Release the db-conn.\nRelease the config.\n"))
  (is (= (count @release-fns)
         0)))

;-------------------------------------------------------------------------------

(deftest missing-def
  (try
    (eval '(do (use 'maker.core)
               (defgoal a [b])
               (make a)))
    (is false)
    (catch Throwable ei
      (is (-> ei ex-data :goal-var :goal-local (= 'b)))))
  (try
    (eval '(do (use 'maker.core)
               (defgoal<> a [b])
               (make<> a)))
    (is false)
    (catch Throwable ei
      (is (-> ei ex-data :goal-var :goal-local (= 'b))))))

;-------------------------------------------------------------------------------

(defgoal e1
  []
  (throw (ex-info "oops" {})))

(defgoal e2
  [e1]
  "ok")

(defgoal<> e3
  [e2]
  "ok")

(deftest error-handling
  (is (thrown? Throwable
               (make e2)))

  (is (thrown? Throwable
               (deref?? (make<> e3)))))


