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

(defn simple*
  []
  "simple")

;; simple is called a 'goal'
;; simple* is the 'maker function
;; "simple" is the value of the goal

(deftest test-simple
  (is (= "simple"
         ;let's make the goal
         (make simple)
         ;the make macro expands to
         (let [simple (simple*)]
           simple))))

;; to define a maker function just puts '*' at the end of name.
(defn other*
  [simple]
  (str simple "-other"))

;; the defgoal macro put '*' at the end of the maker function
(defgoal another
  "Just another goal but now using the defgoal - the same effect."
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

;; the next is a declaration of a goal
(defgoal? iterator-item)

(defn iterator-items*
  []
  (range 10))

(defn collected-item*
  [factor iterator-item]
  (* iterator-item factor))

;;Look at iterator-item, it was declared above and the actual value is defined
;;here locally.
(defgoal collected-items
  "A goal with a make call in it."
  [iterator-items]
  (map (fn [iterator-item]                                  ;binds to iterator-item goal, declared above
         (make collected-item))
       iterator-items))

(deftest test-static-collectors
  (is (= (last (make collected-items))
         18))
  ;;factor* was called 10 times, usually this is not what you want...so read on.
  (is (= @call-counter 10)))

;;The new thing here is the dynamic/implicit parameter list. Maker checks the
;;first parameter, if it is '?' then it extends the parameter list with the
;;neccessary dependency goals. Check with macroexpansion: 'factor' appears as an
;;implicit dependency and created in an upper level.
;;Compare with collected-items above.
(defgoal another-collected-items
  "A goal with an implicit dependency: 'factor'"
  [? iterator-items]
  (map (fn [iterator-item]
         (* 2 (make collected-item)))
       iterator-items))

(deftest test-for-vector
  (reset! call-counter 0)
  ;;expand the make below, the creation of factor is in the right place now.
  (is (= (last (make another-collected-items))
         36))
  ;; despite it is required inside the iteration
  ;;'factor' is made in the 'right' place and called only once
  (is (= @call-counter 1)))

;-------------------------------------------------------------------------------

(defn m* [] {:a 1 :b 2})
(defn v* [] [11 22])

;; maker works together with destructuring
(defn destr-goal*
  [{:keys [a b] :as m} [c :as v]]
  [a b m c v])

(deftest test-destr
  (is (= (make destr-goal)
         [1 2 {:a 1 :b 2} 11 [11 22]])))

;; destructuring wiht dynamic goals

(defn d-destr-goal*
  [{:keys [a b] :as dm} [c :as dv]]
  [a b dm c dv])

(defgoal? dm)
(defgoal? dv)

(deftest test-d-destr
  (is (= (let [dm {:a 111 :b 222}
               dv [1 2]]
           (make d-destr-goal))
         [111 222 {:a 111 :b 222} 1 [1 2]])))

;-------------------------------------------------------------------------------

;; circular dependency is an error at compile time

(deftest circular-dep
  (is (thrown-with-msg? Throwable #"ircular"
                        (eval '(do
                                 (use 'maker.core)
                                 (defn self*
                                   [self])
                                 (make self))))))

;-------------------------------------------------------------------------------
;;An async example.

(defgoal? n)

;;For every defgoal<> the return value has to be a channel and maker's duty to
;;to unwrap the value from it properly.
(defgoal<> urls
  "This goal is defined as a content of a channel."
  [n]
  (let [res (a/promise-chan)]
    (future
      (try
        (->> (range n)
             (map (partial str "url"))
             (a/put! res))
        (catch Throwable th
          (a/put! res th))))
    res))

(defgoal? url)

;;For defgoal<- the return value is given by calling the yield callback.
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

;;make<> will resolve the dependencies asynchronously, in parallel and returns
;;a channel.
(deftest test-async
  (let [n 100
        m 100]
    (is (= (repeat m n)
           (->> (range m)
                (map (fn [_] (future (-> contents make<> a/<!! count))))
                (map deref))))))

;-------------------------------------------------------------------------------
;simple async

(defgoal<> single-async
  []
  (let [ch (a/promise-chan)]
    (a/put! ch 1)
    ch))

(deftest test-single-async
  (is (= 1 (a/<!! (make<> single-async)))))

;-------------------------------------------------------------------------------
;Example support for reloaded framework.

(def stop-fns (atom (list)))

(def stop-fn
  (partial swap! stop-fns conj))

(defn stop-system
  []
  (doseq [f @stop-fns]
    (f)
    (swap! stop-fns rest)))

;The three things above consist the generic support for a reloaded workflow.
;Find below the sample 'components'.

(defgoal config
  []
  (stop-fn #(println "stop the config."))
  "the config")

(defgoal db-conn
  [config]
  (stop-fn #(println "stop the db-conn."))
  (str "the db-conn"))

(deftest my-little-component-framework
  (is (= (make db-conn)
         "the db-conn"))
  (is (= (count @stop-fns)
         2))
  (is (= (with-out-str
           (stop-system))
         "stop the db-conn.\nstop the config.\n"))
  (is (= (count @stop-fns)
         0)))

;-------------------------------------------------------------------------------

(deftest missing-def
  (try
    (eval '(do (use 'maker.core)
               (defgoal a [b])
               (make a)))
    (is false)
    (catch Throwable ei
      (is (-> ei ex-data :goal-map :goal-local (= 'b)))))
  (try
    (eval '(do (use 'maker.core)
               (defgoal<> a [b])
               (make<> a)))
    (is false)
    (catch Throwable ei
      (is (-> ei ex-data :goal-map :goal-local (= 'b))))))

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
  (is (thrown-with-msg? Throwable #"oops"
                        (make e2)))
  (is (thrown-with-msg? Throwable #"async"
                        (eval '(do (use 'maker.core)
                                   (defgoal<> a [])
                                   (make a)))))

  (is (thrown? Throwable
               (take?? (make<> e3)))))

;-------------------------------------------------------------------------------

(deftest munge-test
  (are [s res] (-> s inj-munge (= res))
               "aa" "aa"
               "a.b/c" "a_b!c"
               "ab/c" "ab!c"))

