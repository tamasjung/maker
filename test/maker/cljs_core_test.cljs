(ns maker.cljs-core-test
  (:require [cljs.test :refer [run-tests] :refer-macros [deftest is testing run-tests]]
            [maker.core-test]))

#_(deftest a-test (is false))

(cljs.test/run-tests 'maker.core-test)


