(defproject maker "2.2.0"
  :description "Clojure library to explore inversion of control technique - in several senses."
  :url "https://github.com/tamasjung/maker"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "0.4.500"]]
  :source-paths ["src"]
  :profiles {:dev {:dependencies [[criterium "0.4.5"]
                                  [org.clojure/test.check "1.0.0"]]}}
  :deploy-repositories {"releases" {:url "https://repo.clojars.org" :creds :gpg
                                    :sign-releases false}})
