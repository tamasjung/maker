(ns maker.cljs-core
  (:require #?(:clj [maker.core :as m])
            [cljs.analyzer :as an]
            [cljs.analyzer.api :as an-api]))



(require '[clojure.pprint :refer [pprint]])                 ;FIXME remove


(defn cljs-meta
  [var]
  #_(pprint ["cljs-meta" var] *err*)
  (let [[ns the-name] (-> var :name ((juxt (comp symbol namespace) name)))]
    (-> var
        :meta
        (assoc :name the-name
               :ns ns
               :arglists (-> var :meta :arglists second)))))

(defn cljs-ns-resolve
  [ns-sym sym]
  (or (an-api/ns-resolve ns-sym sym)
      (when-let [syms-ns (-> (an-api/current-state) deref ::an/namespaces ns-sym :uses sym)]
        ;FIXME rename is not covered here
        (an-api/ns-resolve syms-ns sym))))


(def cases-map-atom (atom {}))

(defmacro with-cljs-bindings
  [& body]
  `(binding [m/*meta-fn* cljs-meta
             m/*ns-resolve-fn* cljs-ns-resolve
             m/*ns-fn* #(ns-name *ns*)
             m/*ns-name-fn* identity
             m/*goal-var-to-cases-fn* #(or
                                         #_(do (pprint ["gvtc" @cases-map-atom] *err*) nil)
                                         (@cases-map-atom %)
                                         (throw (ex-info (str "Missing case for" %) {:case %})))]
     ~@body))

(defmacro make
  [goal-name]
  #_(pprint [*ns* &env] *err*)
  ;FIXME prone to unbinding if some part is lazy, consider other mechanisms for replacing these fns
  (with-cljs-bindings
    (m/make-with goal-name (-> &env :locals))))

(defmacro defgoalfn
  [& args]
  (with-cljs-bindings
    (apply (resolve 'maker.core/defgoalfn) nil nil args)))




(defmacro defmulticase
  "Defines a multi case goal with its name and the dispatch goal's name."
  {:arglists '[goal-name docstring? dispatch-goal-name]}
  [goal-name & fdecl]
  (let [{:keys [doc dispatch-goal-name]} (m/args-map [[:doc string?]
                                                      [:dispatch-goal-name symbol?]]
                                                     fdecl)]

    (assert dispatch-goal-name)
    `(defn ~(vary-meta (#'m/maker-fn-name-with-goal-meta goal-name)
                       assoc
                       ::m/multicase true)
       ;FIXME why fn?
       ~@(m/rebuild-args doc [dispatch-goal-name]
                         [(list 'throw '(ex-info "Placeholder only, this function should not be called directly." {}))]))))

(defmacro register-case
  ([multigoal dispatch-value case-goal]
   (with-cljs-bindings
     #_(pprint ["rc" @cases-map-atom] *err*)
     (swap! cases-map-atom assoc-in [(m/goal-sym-goal-var (m/*ns-fn*) multigoal)
                                     dispatch-value]
            (m/goal-sym-goal-var (m/*ns-fn*) case-goal))
     nil))
  ([dispatcher case-goal]
   (apply (resolve 'maker.cljs-core/register-case) nil nil dispatcher case-goal [case-goal])))

(defmacro defcasegoal
  [multi-goal dispatch-value & args]
  (let [case-goal (-> (str multi-goal "-" dispatch-value) (#'m/free-text-to-symbol-chars) symbol)]
    `(do (m/defgoal ~case-goal ~@args)
         (register-case ~multi-goal ~dispatch-value ~case-goal))))

(defmacro with-config
  [& args]
  (with-cljs-bindings
    (apply (resolve 'm/with-config) nil nil args)))


