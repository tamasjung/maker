(ns maker.core
  (:require [clojure.string :as string]
            [maker.graph :as graph]))

(defn inj-munge
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
  [goal-map]
  (let [{:keys [ns name]} (-> goal-map :goal-var meta)]
    (when name
      (->> [ns name]
           (string/join "/")
           symbol))))

(defn goal-map-goal-ns-symbol
  "Calculates the goal's namespace qualified symbol"
  [{:keys [goal-local goal-var] :as _goal-map}]
  (let [{:keys [ns]} (meta goal-var)]
    (when goal-local
      (->> [ns goal-local]
           (string/join "/")
           symbol))))

(defn goal-param-goal-local
  [context-ns ns name]
  (if (ns-resolve context-ns (symbol (with-maker-postfix name)))
    (symbol name)
    (-> [ns name]
        (->> (string/join "/"))
        inj-munge
        symbol)))

(defn goal-var-goal-map
  [context-ns goal-var]
  {:goal-var goal-var
   :context-ns context-ns
   :goal-local (goal-param-goal-local                       ;FIXME wrong fn name?
                 context-ns
                 (-> goal-var meta :ns)
                 ;FIXME without then with
                 (-> goal-var meta :name str without-maker-postfix))})

(defn goal-sym-goal-map
  [context-ns ns goal-sym]
  (when goal-sym
    (let [referred-goal-name (with-maker-postfix goal-sym)
          goal-var (->> referred-goal-name
                        symbol
                        (ns-resolve ns))]
      (if-not goal-var
        (throw (ex-info (str "Unknown goal " goal-sym)
                        {:ns (ns-name ns)
                         :goal-sym goal-sym}))
        (goal-var-goal-map context-ns goal-var)))))

(defn goal-param-goal-map
  [context-ns ns goal-param]
  (when goal-param
    (goal-sym-goal-map context-ns ns (whole-param goal-param))))

(defn goal-map-dep-goal-maps
  "Reads the deps from the goal's meta"
  [{:keys [context-ns goal-var]}]
  (let [ns (-> goal-var meta :ns)]
    (->> goal-var
         meta
         :arglists
         first
         (map (partial goal-param-goal-map context-ns ns)))))

(defn goal-realisation
  [_ctx goal-map]
  (:goal-local goal-map))

(defmulti goal-maker-call (fn [_ctx _end-goal-map goal-map]
                            (when (-> goal-map
                                      :goal-var
                                      meta
                                      ::multicase
                                      true?)

                              :multicase)))

(defmethod goal-maker-call :default
  [{:keys [goal-realisation-fn] :as ctx} _ goal-map]
  `(~(goal-maker-symbol goal-map)
     ~@(map (partial goal-realisation-fn ctx) (goal-map-dep-goal-maps goal-map))))

(defn- dependencies
  [goal-var]
  (let [{:keys [ns arglists] :as meta} (meta goal-var)]
    (when (next arglists)
      ;multiarity is ambigious
      (throw (ex-info (str "Multi-arity is not supported: " (:name meta)) {:meta meta})))
    (->> arglists
         first
         (map (fn [goal-param]
                (or (->> goal-param
                         whole-param
                         with-maker-postfix
                         symbol
                         (ns-resolve ns))
                    (throw (ex-info (str "Undefined dependency: " goal-param)
                                    {:of meta}))))))))

(defn- dependencies-in-ctx
  [given-fn? goal-var]
  (remove given-fn? (dependencies goal-var)))

(defn- sorted-goal-vars
  [stop? {:keys [context-ns]} goal-symbol env-keys]
  (let [to-local-keys (->> env-keys
                           (map #(vector (->> % with-maker-postfix symbol (ns-resolve context-ns))
                                         %))
                           ;vars
                           (filter first)
                           (into {}))
        goal-var (or (->> goal-symbol with-maker-postfix symbol (ns-resolve context-ns))
                     (throw (ex-info (str "Undefined goal: " goal-symbol) {})))]
    (graph/topsort-component (partial dependencies-in-ctx to-local-keys)
                             stop?
                             goal-var)))

(defn render-let
  [local-defs]
  `(let [~@(->> local-defs
                (drop-last 2))]
     ~(->> local-defs
           (take-last 2)
           second)))

(defn- local-defs-for
  "Make a goal out of the environment"
  [{:keys [goal-maker-call-fn context-ns] :as ctx} goal-sym env]
  (let [goal-map (goal-sym-goal-map context-ns context-ns goal-sym)
        goal-vars-sorted (sorted-goal-vars #(-> % meta ::dispatch-goal true?) ;stop early at a dispatch goal, inclusively
                                           ctx
                                           goal-sym
                                           (-> env
                                               keys
                                               set))


        sorted-goal-maps (map (partial goal-var-goal-map context-ns) goal-vars-sorted)
        local-defs (->> sorted-goal-maps
                        (mapcat (juxt :goal-local
                                      #(goal-maker-call-fn ctx goal-map %)))
                        (into []))
        ;FIXME too implicit and needlessly limited
        undefined-goals (remove (comp :arglists meta :goal-var) sorted-goal-maps)]
    (when (seq undefined-goals)
      (throw (ex-info (str "Undefined goals: " (string/join ", " (map :goal-local undefined-goals)))
                      {:goals (mapv :goal-local undefined-goals)
                       :local-defs (->> undefined-goals
                                        (partition-all 2)
                                        (map vec))
                       :for goal-map})))
    local-defs))

(defn make-with
  [ctx goal-sym env]
  (-> (local-defs-for ctx goal-sym env)
      render-let))

(defmacro make
  [goal]
  (make-with {:goal-maker-call-fn goal-maker-call
              :goal-realisation-fn goal-realisation
              :context-ns *ns*}
             goal
             &env))

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
        context-ns-name (-> *ns* ns-name)
        config-key-maps (->> config-keys
                             (map #(let [ns-sym (-> % namespace symbol)]
                                     (hash-map :config-key %
                                               :ns-sym ns-sym
                                               :name (-> % name with-maker-postfix symbol)
                                               :goal-local (if (= ns-sym context-ns-name)
                                                             ;TBD is this discrepancy fine?
                                                             (-> % name symbol)
                                                             (goal-param-goal-local *ns*
                                                                                    ns-sym
                                                                                    (-> % name symbol)))))))
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

;FIXME could this be simpler?
(defn args-map
  [definitions args-in]
  (loop [rest-defintions definitions
         structured-args {}
         args args-in]
    (if-not (seq rest-defintions)
      (assoc structured-args :body (seq args))
      (let [[next-arg & next-args] args]
        (if-not next-arg
          (assoc structured-args :body nil)
          (let [[_skipped-defs [definition & remaining-definitions]]
                (split-with #((complement (second %)) next-arg) rest-defintions)]
            (if-not definition
              (assoc structured-args :body (seq args))
              (recur remaining-definitions
                     (assoc structured-args (first definition) next-arg)
                     next-args))))))))

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
  [goal-maps]
  (->> goal-maps
       (remove #(identical? *ns* (-> % :goal-var meta :ns)))
       (map #(let [the-name (-> % :goal-var meta :name)]
               (list 'refer `(quote ~(-> % :goal-var meta :ns ns-name))
                     :only `(quote [~the-name])
                     :rename `(quote ~{the-name (-> % :goal-local with-maker-postfix symbol)}))))))

;FIXME TBD it is possible without refer. A generic closure would be better.
(defmacro defgoalfn                                         ;better name? dash or not dash
  [name & args]
  (let [{:keys [doc params body goal-sym]} (args-map [[:doc string?]
                                                      [:params vector?]
                                                      [:goal-sym symbol?]]
                                                     args)
        param-goal-maps (map (partial goal-param-goal-map *ns* *ns*) params)
        ;param-map entry: [param-goal-map param]
        param-map (->> (map vector param-goal-maps params)
                       (into {}))
        base-goal-map (goal-sym-goal-map *ns* *ns* goal-sym)
        deps-goal-maps (goal-map-dep-goal-maps base-goal-map)
        additional-param-goal-maps (remove (set param-goal-maps) deps-goal-maps)]
    `(do
       ~@(build-refers-for additional-param-goal-maps)
       (defgoal ~(vary-meta name assoc ::defgoalfn true)
         ~@(concat
             (rebuild-args doc (->> additional-param-goal-maps
                                    (mapv :goal-local))
                           body)
             [`(fn ~params
                 (~(-> goal-sym with-maker-postfix symbol)
                   ~@(map #(or (param-map %)
                               (:goal-local %)) deps-goal-maps)))])))))

(defmacro defgoal?
  [name]
  (list 'declare (maker-fn-name-with-goal-meta name)))

(def cases-map-atom (atom {}))

(defmacro defmulticase
  "Defines a multi case goal with its name and the dispatch goal's name."
  {:arglists '[goal-name docstring? dispatch-goal-name]}
  [goal-name & fdecl]
  (let [{:keys [doc dispatch-goal-name]} (args-map [[:doc string?]
                                                    [:dispatch-goal-name symbol?]]
                                                   fdecl)]

    (assert dispatch-goal-name)
    `(defn ~(vary-meta (maker-fn-name-with-goal-meta goal-name)
                       assoc
                       ::multicase true)
       ;FIXME why fn?
       ~@(rebuild-args doc [dispatch-goal-name]
                       [(list 'throw '(ex-info "Placeholder only, this function should not be called directly." {}))]))))

(defmacro make-case
  ;it is a macro to have env but is that enough reason? env could be propagated (as context-ns is)
  [ctx multi-goal end-goal case-goal]
  (let [local-defs-for-case-goal (local-defs-for ctx case-goal &env)
        multi-goal-map (goal-sym-goal-map *ns* *ns* multi-goal)
        case-goal-map (goal-sym-goal-map *ns* *ns* case-goal)
        local-defs (into local-defs-for-case-goal (map :goal-local [multi-goal-map case-goal-map]))]
    `(let ~local-defs
       ~(make-with ctx end-goal &env))))

(defmethod goal-maker-call :multicase
  [{:keys [goal-realisation-fn] :as ctx} end-goal-map goal-map]
  (let [cases (@cases-map-atom goal-map)
        dispatch-goal-map (-> goal-map goal-map-dep-goal-maps first)]

    `(case ~(goal-realisation-fn ctx dispatch-goal-map)

       ~@(mapcat (fn [[case-item case-goal-map]]
                   (list case-item `(make-case ~ctx         ;FIXME strange, this takes the advantage of that macro result is not serialised to code but remains a memory object, ctx should be a serializable thing to support gradual/partial macro expansion in IDEs.
                                               ~@(map goal-map-goal-ns-symbol
                                                      [goal-map end-goal-map case-goal-map]))))
                 cases))))

(defmacro register-case
  ([dispatcher dispatch-value case-goal]
   `(swap! cases-map-atom assoc-in [(goal-sym-goal-map *ns* *ns* '~dispatcher)
                                    ~dispatch-value]
           (goal-sym-goal-map *ns* *ns* '~case-goal)))
  ([dispatcher case-goal]
   `(register-case ~dispatcher ~case-goal ~case-goal)))
