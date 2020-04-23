(ns maker.core-test
  (:require [clojure.test :refer :all]
            [maker.core :as m :refer :all]
            [clojure.pprint :refer [pprint]]
            [ns2 :refer [ns2a*]]
            [ns1 :refer [ns1a*]]
            [clojure.core.async :as a]
            [clojure.spec.test.alpha :as stest]
            [maker.core-spec])
  (:import (clojure.lang ExceptionInfo)))

;-------------------------------------------------------------------------------

(stest/instrument `make-internal)
(stest/instrument `has-result)

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

;; to define a maker function just put a '*' at the end of name.
(defn other*
  [simple]
  (str simple "-other"))

;; the defgoal macro puts '*' at the end of the goal name
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
  ;with-goals is an extended let, works well with goals from another namespace.
  (is (= 22 (with-goals [ns1a 11]
              (make ns2a)))))

;-------------------------------------------------------------------------------

(declare dd*)

(defn goal-with-dyn-dep* [dd] (+ 10 dd))

(deftest test-dynamic-goal
  (is (= (let [dd 1]
           (make goal-with-dyn-dep))
         11)))

;-------------------------------------------------------------------------------

(def call-counter (atom 0))

(defn factor*
  []
  (swap! call-counter inc)
  2)

;; the next is a declaration of a goal without definition
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
;;necessary dependency goals. Check with macroexpansion: 'factor' appears as an
;;implicit dependency and created in an upper level.
;;Compare with collected-items above.

(defgoal another-collected-items
  "A goal with an implicit dependency: 'factor'"
  [? iterator-items]
  (map (fn [iterator-item]
         (* 2 (make collected-item)))
       iterator-items))


(defgoalfn collected-item-fn [iterator-item] collected-item)
;=>
#_(do (refer 'collected-items-ns
             :rename {d-1 collected-item-fn-collected-items-ns-d1}
             :only [d-1])
      (refer 'transitive-dep-ns
             :rename {d-2 d-2-34356}
             :only [d-2])
    (defn collected-item-fn
      [d-1 d-2]
      (fn [iterator-items]
        (collected-item* d-1 d-2))))
#_
(defn yet-another-collected-items2
  [iterator-items  collected-item-fn]
  (map collected-item-fn iterator-items))
#_
(defgoal yet-another-collected-items3
  [iterator-items ^{:m/goal-fn '[collected-item [iterator-item]]} $]
  (map $ iterator-items))

#_
(defgoal yet-another-collected-item4
  [iterator-items ^:as-goal-fn collected-item])



(deftest test-for-vector
  (reset! call-counter 0)
  ;;expand the make below, the creation of factor is in the right place now.
  (is (= (last (make another-collected-items))
         36))
  ;; despite it is required inside the iteration
  ;;'factor' is made in the 'right' place and called only once
  (is (= @call-counter 1)))

(deftest test-spec
  (eval '(do (use 'maker.core)
             (defgoal sb [])
             (defgoal sa [sb] 1)
             (make sa))))

;-------------------------------------------------------------------------------

(def choice-env*)

(defn choice-dep-a*
  []
  1)

(defn choice-dep-b*
  []
  2)


(defn ^{::m/goal-type ::m/case} choice*
  [choice-env]
  choice-env)

(defn choice1*
  [choice-dep-a]
  (str choice-dep-a))

(register-case choice :choice1 choice1)

(defn choice2*
  [choice-dep-b]
  (str choice-dep-b))

(register-case choice :choice2 choice2)

; b/c choice has the goal type m/case meta, the expansion of creating it will be
; (case ..) and the return value of 'choice' is used as the dispatcher and the
; matching cohice (in our case choice1) will be 'made'.
; Check the expansion of make below.
(deftest choice-test
  (is (= (let [choice-env :choice1]
           (make choice))
         "1")))

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
  (is (re-find #"Circular dependency"
               (try

                 (eval '(do
                          (use 'maker.core)
                          (defn self*
                            [self])
                          (make self)))
                 (catch Throwable th
                   (str th))))))

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

;;For defgoal<- the return value is given by the yield callback.
(defgoal<- content
  [url]
  (future
    (try
      (yield (str url "content"))
      (catch Throwable th
        (yield th)))))

(defgoal<> contents
  [urls]
  (let [res-ch (a/chan 1000)]
    (a/pipeline-async 10
                      res-ch
                      (fn [url result-ch]
                        (a/go (a/>! result-ch (a/<! (result-chan (make<> content))))
                              (a/close! result-ch)))
                      (a/to-chan urls))
    (a/into [] res-ch)))

;;make<> will resolve the dependencies asynchronously in parallel and returns
;;a channel.
(deftest test-async
  (let [n 100
        m 10]
    (is (= (range m)
           (->> (range m)
                (map (fn [n] (future (-> contents make<> (take-in?? 10000)
                                         count))))
                (map deref))))
    (is (= (count (make urls))
           n)))
  (let [url "u"]
    (is (= (make content)
           "ucontent"))))

;-------------------------------------------------------------------------------
;simple async

(defgoal<> single-async
  []
  (let [ch (a/promise-chan)]
    (a/put! ch 1)
    ch))

(deftest test-single-async
  (is (= 1 (take-in?? (make<> single-async) 1000))))

;-------------------------------------------------------------------------------
;async handling of multiple errors

(defgoal<- err0
  []
  (future
    (Thread/sleep 100)
    (yield (ex-info "Err0" {:i 0}))))


(defgoal<> err1
  []
  (let [ch (a/promise-chan)]
    (future
      (Thread/sleep 200)                                    ;make this the second error
      (a/put! ch (ex-info "Err1" {:i 1})))
    ch))


(defgoal<> err-result
  [err0 err1]
  (a/to-chan [err0 err1]))


(deftest test-two-errors
  (let [[_ ctx-agent :as res] (make<> err-result)]
    (is (thrown? ExceptionInfo (take-in?? res 10000)))
    (Thread/sleep 200)
    (is (= #{"Err0" "Err1"}
           (->> ctx-agent deref :errors (map ex-message) set)))
    (is (= "Err1" (->> ctx-agent deref :result second a/<!! ex-message)))))

;-------------------------------------------------------------------------------
;Example support for reloaded framework.

(def stop-fns (atom ()))

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
  (is (re-find #"Unknown goal"
               (try
                 (eval '(do (use 'maker.core)
                            (defgoal a [b])
                            (make a)))
                 (catch Throwable ei
                   (-> ei (.getCause) str)))))

  (try
    (eval '(do (use 'maker.core)
               (defgoal<> aa [bb])
               (clojure.core.async/<!! (make<> aa))))
    (is false)
    (catch Throwable ei
      (is (= 'bb (-> ei (.getCause) ex-data :goal-param)))))

  (try
    (eval '(do (use 'maker.core)
               (defgoal? bbb)
               (defgoal<> aaa [bbb])
               (take-in?? (make<> aaa) 1000)))
    (is false)
    (catch Throwable ei
      (is (= 'bbb (-> ei ex-data :undefineds ffirst)))))

  (is (= '[aaaa]
         (try
           (eval '(do (use 'maker.core)
                      (defgoal? aaaa)
                      (make aaaa)
                      nil))
           (catch Throwable ei
             (-> ei (.getCause) ex-data :goals))))))

;-------------------------------------------------------------------------------

(defgoal e1
  []
  (throw (ex-info "oops" {})))

(defgoal e2
  [e1]
  "ok")

(defgoal<> e3
  [e2]
  (a/promise-chan "it would be not ok to see this"))

(deftest error-handling
  (is (thrown-with-msg? Throwable #"oops"
                        (make e2)))

  (is (thrown-with-msg? Throwable #"Value is Throwable"
                        (take?? (make<> e3)))))

;-------------------------------------------------------------------------------

(defgoal? g0)

(defgoal g1 [g0] 1)

(defgoal g2
  [?]
  (make g1)
  (make g1))

(deftest no-double-param
  (is (= (-> g2* var meta :arglists first)
         ['g0])))

;-------------------------------------------------------------------------------
(deftest munge-test
  (are [s res] (-> s inj-munge (= res))
               "aa" "aa"
               "a.b/c" "a_b!c"
               "ab/c" "ab!c"))

