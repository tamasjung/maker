(ns maker.spec-test
  (:require [clojure.test :refer :all]
            [maker.spec :as ms]
            [maker.core :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest])
  (:import (clojure.lang ExceptionInfo)))

(defgoal? a)

(ms/def a int?)

(defgoal b [a] "b")

(ms/fdef b string?)

;;for no spec it will be `any?` by default instead of a NPE
(defgoal nospec [])


(defgoal c
  [a b nospec]
  (str a b))

(ms/fdef c string?)

(stest/instrument `c')
(stest/instrument `b')

(deftest doal-specs
  (is (= "1b"
         (let [a 1]
           (make c))))
  (is (thrown-with-msg? ExceptionInfo #"not conform"
                        (let [a "a"]
                          (make c)))))