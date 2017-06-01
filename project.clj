(defproject maker "1.0.1-SNAPSHOT"
  :description "Maker is a macro for resolving dependencies between plain functions."
  :url "https://github.com/tamasjung/maker"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :source-paths ["src"]
  :profiles {:dev {:dependencies [[org.clojure/core.async "0.3.442"]
                                  [criterium "0.4.4"]
                                  [manifold "0.1.7-alpha2"]]}})
