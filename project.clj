(defproject maker "2.0.0"
  :description "Maker is a macro for resolving dependencies between plain functions."
  :url "https://github.com/tamasjung/maker"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.442"]]
  :source-paths ["src"]
  :profiles {:dev {:dependencies [[criterium "0.4.4"]]}})
