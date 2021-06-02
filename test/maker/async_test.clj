(ns maker.async-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :as a]
            [clojure.pprint :refer [pprint]]
            [maker.async :refer :all]
            [maker.core :refer :all]
            [maker.dev :refer :all])
  (:import (clojure.lang ExceptionInfo)))

;-------------------------------------------------------------------------------

;;An async example.
(defgoal? n)

;;For every defgoal> the return value has to be a channel and maker's duty to
;;to unwrap the value from it properly.
(defgoal> urls
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

;;For defgoal< the return value is given by the yield callback.
(defgoal< content
  [url]
  (future
    (try
      #_(Thread/sleep 1000)
      (yield (str url "content"))
      (catch Throwable th
        (yield th)))))

(defgoalfn content-fn [url] content)

(defgoal> contents
  [content-fn urls]
  (let [res-ch (a/chan 1000)]
    (a/pipeline-async 100
                      res-ch
                      (fn [url result-ch]
                        (a/go (a/>! result-ch (a/<! (content-fn url)))
                              (a/close! result-ch)))
                      (a/to-chan!! urls))
    (let [res (a/promise-chan)]
      (a/pipe (a/into [] res-ch) res)
      res)))

;;make> will resolve the dependencies asynchronously in parallel and returns
;;a channel.
(deftest test-async
  (let [n 100]
    (is (= (-> urls make> (take-in!! 1000) count)
           n)))
  (let [url "u"]
    (is (= (-> (make content) (take-in!! 1000))
           "ucontent"))))

(deftest test-parallel
  (let [m 100]
    (is (= (range m)
           (->> (range m)
                (mapv (fn [n] (future (-> (make> contents) (take-in!! 1000)
                                          count))))
                (mapv deref)))))
  )

;-------------------------------------------------------------------------------
;simple async

(defgoal> single-async
  []
  (let [ch (a/promise-chan)]
    (a/put! ch 1)
    ch))

(deftest test-single-async
  (is (= 1 (take-in!! (make single-async) 1000)))
  (is (= 1 (take-in!! (make> single-async) 1000))))

;-------------------------------------------------------------------------------

(defgoal single-sync
  []
  11)

(deftest test-single-sync
  (is (= 11 (-> (make> single-sync) (take-in!! 1000)))))

;-------------------------------------------------------------------------------
;async handling of multiple errors

(defgoal< err0
  []
  (future
    (Thread/sleep 100)
    (yield (ex-info "Err0" {:i 0}))))


(defgoal> err1
  []
  (let [ch (a/promise-chan)]
    (future
      (Thread/sleep 200)                                    ;make this the second error
      (a/put! ch (ex-info "Err1" {:i 1})))
    ch))


(defgoal> err-result
  [err0 err1]
  (pipe-to-promise-chan (a/to-chan! [err0 err1])))


(deftest test-two-errors
  (let [res (make> err-result)]
    (is (thrown-with-msg? ExceptionInfo #"Err0|Err1" (take-in!! res 1000)))))

(defn max-mem'
  []
  (.. (Runtime/getRuntime) (maxMemory)))

(defn one-gig'
  [max-mem]
  (byte-array (quot max-mem 5)))

(defgoal< a-error
  [one-gig]
  (yield (ex-info "Eh" {})))

(deftest async-mem-leak

  (dotimes [_ 10]
    (is (thrown? ExceptionInfo (take-in!! (make> a-error) 100)))))

(deftest missing-def
  (try
    (eval '(do (ns ns-missing-def1
                 (:require [maker.async :refer [defgoal> make>]]))
               (defgoal> aa [bb])
               (clojure.core.async/<!! (make> aa))))
    (is false)
    (catch Throwable ei
      (is (= 'bb (-> ei (.getCause) ex-data :of :arglists ffirst)))))

  (try
    (eval '(do (ns ns-missing-def2
                 (:require [maker.async :refer [defgoal> take-in!! make>]]
                           [maker.core :refer [defgoal?]]))
               (defgoal? bbb)
               (defgoal> aaa [bbb])
               (take-in!! (make> aaa) 1000)))
    (is false)
    (catch Throwable ei
      (is (= 'bbb' (-> ei (.getCause) ex-data :meta :name))))))


(defgoal e1
  []
  (throw (ex-info "oops" {})))

(defgoal e2
  [e1]
  "ok")

(defgoal> e3
  [e2]
  (doto (a/promise-chan)
    (a/put! "it would be not ok to see this")))

(deftest error-handling
  (is (thrown-with-msg? Throwable #"oops"
                        (make e2)))


  (is (thrown-with-msg? Throwable #"oops"
                        (take-in!! (make> e3) 1000))))


