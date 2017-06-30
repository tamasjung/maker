(ns maker.spec-test
  (:require [clojure.test :refer :all]
            [maker.spec :as ms]
            [maker.core :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.future :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(defgoal? ^{:spec int?} a)

(ms/def a int?)

(defgoal b [a] "b")

(ms/fdef b string?)

(defgoal nospec [])

(defgoal c
  [a b nospec]
  (str a b))

(ms/fdef c string?)

(stest/instrument `c*)
(stest/instrument `b*)

(deftest doal-specs
  (is (= "1b"
         (let [a 1]
           (make c))))
  (is (thrown-with-msg? ExceptionInfo #"not conform"
                        (let [a "a"]
                          (make c)))))

