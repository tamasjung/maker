(ns maker.core
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [maker.graph :as graph]
            [clojure.string :as str]))
(require '[clojure.pprint :refer [pprint]])                 ;FIXME remove
;dynamized functions b/c of clj-cljs differences

(def ^:dynamic *meta-fn* meta)

(def ^:dynamic *ns-resolve-fn* ns-resolve #_(fn [n s] #_(pprint ["ooo" n s] *err*) (ns-resolve n s)))

(def ^:dynamic *ns-fn* #(do *ns*))

(def ^:dynamic *ns-name-fn* ns-name)



(defn non-q-sym
  "Injective munge"
  [s]
  (-> s
      (string/replace "+" "++")
      (string/replace "/" "+!")
      (string/replace "." "+_")))

(defn with-maker-postfix
  [s]
  (str s \'))

(defn without-maker-postfix
  [s]
  (when (re-find #"\'$" s)
    (subs s 0 (-> s count dec))))

(defn whole-param
  "Returns the ':as' symbol or itself"
  [dep]
  (cond
    (symbol? dep) dep
    (map? dep) (or (:as dep)
                   (throw (ex-info
                            "Missing dependency name, you may want to add ':as' to the parameter destructuring."
                            {:dep dep})))
    (vector? dep) (let [[as whole] (take-last 2 dep)]
                    (if (and (= :as as)
                             (symbol? whole))
                      whole
                      (throw (ex-info "Unrecognized vector param."
                                      {:dep dep}))))
    :default (throw (ex-info
                      "Unrecognized dependency."
                      {:dep dep}))))

(defn goal-maker-symbol
  "Calculates the goal maker fn's symbol"
  [goal-var]
  (let [{:keys [ns name]} (*meta-fn* goal-var)]
    (when name
      (->> [ns name]
           (string/join "/")
           symbol))))

(defn goal-sym-goal-var
  [ns goal-sym]
  (->> goal-sym
       with-maker-postfix
       symbol
       (*ns-resolve-fn* ns)))

(defn goal-param-goal-local
  [context-ns ns name]
  (if (goal-sym-goal-var context-ns name)
    (symbol name)
    (-> [ns name]
        (->> (string/join "/"))
        non-q-sym
        symbol)))

(defn goal-var-goal-local
  [context-ns goal-var]
  (goal-param-goal-local context-ns
                         (-> goal-var *meta-fn* :ns)
                         (-> goal-var *meta-fn* :name str without-maker-postfix)))

(defn goal-realisation
  [ctx goal-var]
  (goal-var-goal-local (:context-ns ctx) goal-var))

(defn alias-assignment
  [ctx var1 var2]
  ;FIXME direct call??
  [(goal-realisation ctx var1)
   (goal-realisation ctx var2)])

(defn- dependencies
  [goal-var]
  #_(pprint ["deps" goal-var] *err*)
  (let [{:keys [ns arglists] :as var-meta} (*meta-fn* goal-var)]
    (when (next arglists)
      ;multiarity is ambigious
      (throw (ex-info (str "Multi-arity is not supported: " (:name var-meta) arglists) {:meta var-meta})))
    (->> arglists
         first
         (mapv (fn [goal-param]
                 (or (->> goal-param
                          whole-param
                          (goal-sym-goal-var ns))
                     (throw (ex-info (str "Undefined dependency: " goal-param " " var-meta (str "++" ns "++"))
                                     {:of var-meta}))))))))

(defn render-let
  [assignments]
  `(let [~@(->> assignments
                drop-last
                (reduce into []))]
     ~(->> assignments
           last
           second)))

(defn multicase?
  [goal-var]
  (-> goal-var *meta-fn* ::multicase true?))

(defn- take-until
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (list (first s))
        (cons (first s) (take-until pred (rest s)))))))

(defn sorted-deps
  ([state]
   (->> (graph/top-sorting dependencies state)
        (take-until (comp multicase? first))))
  ([state end-goal-var]
   (->> (graph/top-sorting dependencies
                           (-> state
                               (update :stack conj end-goal-var)
                               (update :stack-set conj end-goal-var)))
        (take-until (fn [[v]] (or (multicase? v)
                                  (= end-goal-var v)))))))

(defn- goal-sym-to-multi-registry-name
  [sym]
  (symbol (str sym "-registry")))

(defn- goalvar-to-multi-registry
  [goal-var]
  #_(pprint ["gtmr" goal-var *ns-resolve-fn*] *err*)
  (->> goal-var
       *meta-fn*
       :name
       goal-sym-to-multi-registry-name
       (*ns-resolve-fn* (-> goal-var *meta-fn* :ns))))

(def ^:dynamic *goal-var-to-cases-fn* (comp deref #_the_atom deref #_the_var goalvar-to-multi-registry))

(defmulti goal-model (fn [goal-var _]
                       (when (multicase? goal-var)
                         ::multicase)))

(defn case-model
  [multi-goal-var sorting-state [dispatch-value case-goal-var]]
  #_(pprint ["cm" dispatch-value case-goal-var] *err*)
  (let [case-dep-and-states (sorted-deps sorting-state case-goal-var)
        ;since the case goal is 'sorted' then the multicase-goal is sorted too
        ;let's put into the 'state' manually
        state-with-multi (-> case-dep-and-states last second
                             (update :sorted-set conj multi-goal-var))
        ;finish the algorithm with the 'corrected' state
        end-goal-dep-and-states (sorted-deps state-with-multi)

        case-goal-dep-models (mapv #(apply goal-model %) case-dep-and-states)
        end-goal-dep-models (mapv #(apply goal-model %) end-goal-dep-and-states)]
    {:multi-goal-var multi-goal-var
     :dispatch-value dispatch-value
     :case-goal-model (goal-model case-goal-var
                                  (-> case-dep-and-states
                                      last
                                      second))
     :case-goal-dep-models case-goal-dep-models
     :end-goal-dep-models end-goal-dep-models
     :used-dep-vars (set/intersection
                      (:sorted-set sorting-state)
                      (->> case-goal-dep-models
                           (into end-goal-dep-models)
                           (map :used-dep-vars)
                           (reduce into #{})))}))

(defmethod goal-model ::multicase
  [goal-var sorting-state]
  (let [dispatch-goal-var (->> goal-var dependencies first)
        ;undo the last change in sorting state for multicase
        state-without-multi (-> sorting-state
                                (update :sorted-set disj goal-var))
        case-models (->> goal-var
                         *goal-var-to-cases-fn*
                         (mapv (partial case-model goal-var state-without-multi)))]
    {:goal-var goal-var
     :dispatch-goal-var dispatch-goal-var
     :case-models case-models
     :used-dep-vars (->> case-models
                         (map :used-dep-vars)
                         (reduce into #{}))}))



(defmethod goal-model :default
  [goal-var _sorting-state]
  {:goal-var goal-var
   :used-dep-vars (dependencies goal-var)})

(defmulti render-assignment (fn [_ {:keys [goal-var]}]
                              (cond

                                (multicase? goal-var)
                                ::multicase

                                (-> goal-var *meta-fn* :declared true?)
                                ::declaration

                                (-> goal-var *meta-fn* :arglists nil?)
                                ::direct-def)))

(defmethod render-assignment ::direct-def
  [{:keys [var-to-local-fn]} {:keys [goal-var]}]
  [(var-to-local-fn goal-var)
   `~(goal-maker-symbol goal-var)])

(defmethod render-assignment ::declaration
  [_ {:keys [goal-var]}]
  (throw (ex-info (str "Unbounded goal:" (-> goal-var *meta-fn* :name))
                  {:meta (*meta-fn* goal-var)})))

(defmethod render-assignment :default
  [{:keys [var-to-local-fn goal-realisation-fn] :as ctx} {:keys [goal-var used-dep-vars]}]
  [(var-to-local-fn goal-var)
   `(~(goal-maker-symbol goal-var)
      ~@(mapv (partial goal-realisation-fn ctx) used-dep-vars))])

(defn- free-text-to-symbol-chars
  [txt]
  (subs (str/replace txt #"[^a-zA-Z0-9\*\+\!\-\_\'\?\<\>\=\:]" "-")
        0 (min 100 (count txt))))

(defn render-case-assignment
  [{:keys [var-to-local-fn]
    :as ctx}
   {:keys [used-dep-vars
           case-goal-model
           case-goal-dep-models
           end-goal-dep-models
           multi-goal-var
           dispatch-value]
    :as _case-model}]
  (let [params (mapv var-to-local-fn used-dep-vars)
        ;only for debugging purpose, it will be on the stack-trace
        fn-name (-> (str dispatch-value
                         "-"
                         (var-to-local-fn multi-goal-var)
                         "-fn")
                    free-text-to-symbol-chars
                    gensym)
        assignments (reduce into []
                            [(mapv (partial render-assignment ctx) case-goal-dep-models)
                             ;FIXME wrong if multi is chan
                             [(alias-assignment ctx multi-goal-var (:goal-var case-goal-model))]
                             (mapv (partial render-assignment ctx) end-goal-dep-models)])]
    [dispatch-value
     `((fn ~fn-name [~@params]
         ~(render-let assignments)) ~@params)]))

(defmethod render-assignment ::multicase
  [{:keys [goal-realisation-fn] :as ctx}
   {:keys [dispatch-goal-var case-models]}]
  `['NEVER_SEE                                              ;b/c multicase is the last one
    (case ~(goal-realisation-fn ctx dispatch-goal-var)
      ~@(mapcat (partial render-case-assignment ctx) case-models))])


(defn make-with
  ([goal env]
   (make-with {:render-assignment-fn render-assignment
               :goal-realisation-fn goal-realisation
               :context-ns (*ns-fn*)
               :var-to-local-fn (partial goal-var-goal-local (*ns-fn*))}
              goal
              env))
  ([{:keys [context-ns render-assignment-fn] :as ctx} goal-sym env]
   (let [env-vars (->> env
                       keys
                       (map (partial goal-sym-goal-var context-ns))
                       set)
         end-goal-var (goal-sym-goal-var context-ns goal-sym)
         vars-and-states (sorted-deps {:stack [] :stack-set #{} :sorted-set env-vars}
                                      end-goal-var)
         dep-models (mapv (partial apply goal-model) vars-and-states)]
     (->> dep-models
          (mapv (partial render-assignment-fn ctx))
          render-let))))

(defmacro make
  ([goal]
   (make-with goal &env)))

(defmacro with-config
  "Makes the first parameter as a configuration goal at compile time to extract the keys.
  At runtime it checks if the same keys are present."
  [configs & body]
  ;is it less readable if it is (more) hygenic? is it better?
  ;TODO ^^^
  (let [[compile-time-config-form config-form] (if (vector? configs)
                                                 configs
                                                 [`(keys ~configs) configs])
        config-keys (eval compile-time-config-form)
        context-ns-name (-> (*ns-fn*) *ns-name-fn*)
        config-key-maps (->> config-keys
                             (map #(let [ns-sym (-> % namespace symbol)]
                                     {:config-key %
                                      :ns-sym ns-sym
                                      :name (-> % name with-maker-postfix symbol)
                                      :goal-local (if (= ns-sym context-ns-name)
                                                    ;TBD is this discrepancy fine?
                                                    (-> % name symbol)
                                                    (goal-param-goal-local (*ns-fn*)
                                                                           ns-sym
                                                                           (-> % name symbol)))})))
        refers (->> config-key-maps
                    (remove #(= context-ns-name (:ns-sym %)))
                    (map #(let [the-name (-> % :name)]
                            (list 'refer `(quote ~(-> % :ns-sym))
                                  :only `(quote [~the-name])
                                  :rename `(quote ~{the-name (-> % :goal-local with-maker-postfix symbol)})))))]
    ;TBD could we eliminate this 'hidden' code

    (->> refers
         (map eval)
         doall)
    `(let [~'config ~config-form]
       (when-let [~'missing-keys (->> ~(vec config-keys)
                                      (remove #(contains? ~'config %))
                                      seq)]
         (throw (ex-info (str "Missing config keys " (string/join ", " ~'missing-keys)) {})))
       ;;FIXME comment below and don't render if refers is empty
       (comment "the next 'refer' line(s) would be too late here and called during compile time"
                ~@refers)
       (let ~(->> config-key-maps
                  (map #(list (:goal-local %) (list 'get 'config (:config-key %))))
                  (reduce concat)
                  vec)
         ~@body))))

(defn- maker-fn-name-with-goal-meta
  [name]
  (with-meta (-> name
                 with-maker-postfix
                 symbol)
             (meta name)))

(defn args-map
  ([defs args]
   (args-map defs args {}))
  ([[[k validator] & rest-defs] [next-arg & rest-args :as args] result]
   (if (and validator next-arg)
     (if (validator next-arg)
       (recur rest-defs rest-args (assoc result k next-arg))
       (recur rest-defs args result))
     (assoc result :body args))))

(defn rebuild-args
  [& args]
  (concat (remove nil? (butlast args))
          (last args)))

(defmacro defgoal
  [name & fdecl]
  (let [{:keys [doc params body]} (args-map [[:doc string?]
                                             [:params vector?]]
                                            fdecl)]
    `(defn ~(maker-fn-name-with-goal-meta name)
       ~@(rebuild-args doc params body))))

(defn- build-refers-for
  [goal-vars]
  (->> goal-vars
       (remove #(= (*ns-fn*) (-> % *meta-fn* :ns)))         ;FIXME remove those which already have aliases in the (*ns-fn*) too
       (mapv #(let [the-name (-> % *meta-fn* :name symbol)]
                (list 'require `(quote [~(-> % *meta-fn* :ns *ns-name-fn*)
                                        :refer [~the-name]
                                        :rename ~{the-name (-> (goal-var-goal-local (*ns-fn*) %) with-maker-postfix symbol)}]))))))

;FIXME TBD it is possible without refer. A generic closure would be better.
(defmacro defgoalfn                                         ;better name? dash or not dash
  [name & args]
  ;(pprint ["iiii" name args] *err*)
  (let [{:keys [doc params body goal-sym]} (args-map [[:doc string?]
                                                      [:params vector?]
                                                      [:goal-sym symbol?]]
                                                     args)
        param-goal-vars (map (partial goal-sym-goal-var (*ns-fn*)) params)
        ;param-map entry: [param-goal-map param]
        param-map (->> (map vector param-goal-vars params)
                       (into {}))
        base-goal-var (goal-sym-goal-var (*ns-fn*) goal-sym)
        deps-goal-vars (dependencies base-goal-var)
        additional-param-goal-vars (remove (set param-goal-vars) deps-goal-vars)]
    `(do
       ~@(build-refers-for additional-param-goal-vars)
       (defgoal ~(vary-meta name assoc ::defgoalfn true)
         ~@(concat
             (rebuild-args doc (->> additional-param-goal-vars
                                    (mapv (partial goal-var-goal-local (*ns-fn*))))
                           body)
             [`(fn ~params
                 (~(-> goal-sym with-maker-postfix symbol)
                   ~@(mapv #(or (param-map %)
                                (goal-var-goal-local (*ns-fn*) %)) deps-goal-vars)))])))))

(defmacro defgoal?
  [name]
  (list 'declare (maker-fn-name-with-goal-meta name)))


(defmacro defmulticase
  "Defines a multi case goal with its name and the dispatch goal's name."
  {:arglists '[goal-name docstring? dispatch-goal-name]}
  [goal-name & fdecl]
  (let [{:keys [doc dispatch-goal-name]} (args-map [[:doc string?]
                                                    [:dispatch-goal-name symbol?]]
                                                   fdecl)
        registry-name (-> goal-name
                          with-maker-postfix
                          symbol
                          goal-sym-to-multi-registry-name)]

    (assert dispatch-goal-name)

    `(do
       (def ~registry-name (atom {}))
       (defn ~(vary-meta (maker-fn-name-with-goal-meta goal-name)
                         assoc
                         ::multicase true)
         ;FIXME why fn?
         ~@(rebuild-args doc [dispatch-goal-name]
                         [(list 'throw '(ex-info "Placeholder only, this function should not be called directly." {}))])))))

(defmacro register-case
  ([multigoal dispatch-value case-goal]
   (let [multi-goal-registry (-> (goal-sym-goal-var (*ns-fn*) multigoal)
                                 (goalvar-to-multi-registry)
                                 goal-maker-symbol)]
     `(swap! ~multi-goal-registry
             assoc
             ~dispatch-value
             (goal-sym-goal-var (*ns-fn*) '~case-goal))))
  ([dispatcher case-goal]
   ~(apply (resolve 'maker.core/register-case) nil nil dispatcher case-goal [case-goal])))

(defmacro defcasegoal
  [multi-goal dispatch-value & args]
  (let [case-goal (-> (str multi-goal "-" dispatch-value) free-text-to-symbol-chars symbol)]
    `(do
       (defgoal ~case-goal ~@args)
       ~(apply (resolve 'maker.core/register-case) nil nil multi-goal dispatch-value [case-goal]))))

