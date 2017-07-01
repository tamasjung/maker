(defproject maker "2.0.2-alpha"
  :description "Clojure library to explore inversion of control technique - in several senses."
  :url "https://github.com/tamasjung/maker"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.442"]
                 [clojure-future-spec "1.9.0-alpha17"]]
  :source-paths ["src"]
  :profiles {:dev {:dependencies [[criterium "0.4.4"]
                                  [org.clojure/test.check "0.9.0"]]}})
