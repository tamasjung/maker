(ns maker.dev
  "Developer utilities."
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))


(defmacro trace
  "Macro tracing macro, prints the output or the Throwable."
  [print-fn m & args]
  (let [p-fn (eval print-fn)]
    (try (let [output (apply (resolve m) &form &env args)]
           (p-fn output)
           output)
         (catch Throwable th
           (p-fn th)
           (throw th)))))

(defn ppr
  "Prints a form removing some namespaces and stuff to make it more readable."
  [v]
  (-> (with-out-str (pprint v))
      (str/replace #"clojure.core.*?\/" "")
      (str/replace (re-pattern (str "\\(" (ns-name *ns*) "/")) "(")
      (str/replace #"java\.lang\." "")
      (str/replace #"__\d+__auto__" "#")
      print))

(defmacro ->-
  [m & args]
  `(trace ppr ~m ~@args))
