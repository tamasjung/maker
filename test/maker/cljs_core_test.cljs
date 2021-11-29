(ns maker.cljs-core-test
  (:require [cljs.test :refer [run-tests] :refer-macros [deftest is testing run-tests]]
            [maker.core-test]))

(cljs.test/run-tests 'maker.core-test)


