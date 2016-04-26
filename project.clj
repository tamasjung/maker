(defproject maker "0.1.3-SNAPSHOT"
  :description "Maker is a macro for resolving dependencies between plain functions."
  :url "https://github.com/tamasjung/maker"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :source-paths ["src"]
  :profiles {:dev [:dev-common :dev-local]
             :dev-common {}})
