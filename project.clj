(defproject maker "0.1.2-SNAPSHOT"
  :description "Maker is a macro for resolving dependencies between plain functions."
  :url "https://github.com/tamasjung/maker"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :source-paths ["src"]
  :profiles {:dev [:dev-common :dev-local]
             :dev-common {:plugins [[org.clojure/tools.nrepl "0.2.12"
                                     :exclusions [[org.clojure/clojure]]]
                                    [com.jakemccrary/lein-test-refresh "0.12.0"]]
                          :test-refresh {:changes-only true}
                          :dependencies [[org.clojure/tools.nrepl "0.2.12"]]}})
