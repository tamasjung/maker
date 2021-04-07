(ns maker.core
  (:require [clojure.string :as string]
            [clojure.core.async :as a]
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
  (let [{:keys [ns name]} (:goal-meta goal-map)]
    (when name
      (->> [ns name]
           (string/join "/")
           symbol))))

(defn goal-map-goal-ns-symbol
  "Calculates the goal's namespace qualified symbol"
  [{:keys [goal-local] :as goal-map}]
  (let [{:keys [ns]} (:goal-meta goal-map)]
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
   :goal-local (goal-param-goal-local
                 context-ns
                 (-> goal-var meta :ns)
                 (-> goal-var meta :name str without-maker-postfix))
   :goal-meta (meta goal-var)})

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
        (goal-var-goal-map context-ns goal-var))))
  )

(defn goal-param-goal-map
  [context-ns ns goal-param]
  (when goal-param
    (goal-sym-goal-map context-ns ns (whole-param goal-param))))

(defn goal-map-dep-goal-maps
  "Reads the deps from the goal's meta"
  [context-ns goal-map]
  (let [g-meta (:goal-meta goal-map)
        ns (-> g-meta :ns)]
    (->> g-meta
         :arglists
         first
         (map (partial goal-param-goal-map context-ns ns)))))

(defmulti goal-maker-call (fn [exec-strat goal-map _]
                            [exec-strat
                             (-> goal-map
                                 :goal-meta
                                 ::goal-type)]))

(defmethod goal-maker-call [::sequential ::async-goal-channel]
  [_ goal-map goal-deps]
  `(clojure.core.async/<!! (~(goal-maker-symbol goal-map)
                             ~@(map :goal-local goal-deps))))

(defmethod goal-maker-call [::sequential nil]
  [_ goal-map goal-deps]
  `(~(goal-maker-symbol goal-map)
     ~@(map :goal-local goal-deps)))

(defmethod goal-maker-call [::async ::async-goal-channel]
  [_ goal-map goal-deps]
  `(let [r# (clojure.core.async/promise-chan)]
     (clojure.core.async/go
       (clojure.core.async/>! r#
                              (clojure.core.async/<!
                                (~(goal-maker-symbol goal-map)
                                  ~@(map (fn [{:keys [in-ctx goal-local]}]
                                           (if in-ctx
                                             goal-local
                                             (list 'clojure.core.async/<! goal-local)))
                                         goal-deps)))))
     r#))

(defmethod goal-maker-call [::async nil]
  [_ goal-map goal-deps]
  `(let [r# (clojure.core.async/promise-chan)]
     (clojure.core.async/thread (clojure.core.async/put! r#
                                                         (~(goal-maker-symbol goal-map)
                                                           ~@(map (fn [{:keys [in-ctx goal-local]}]
                                                                    (if in-ctx
                                                                      goal-local
                                                                      (list 'clojure.core.async/<!! goal-local)))
                                                                  goal-deps))))
     r#))

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
  [ns goal-symbol env-keys]
  (let [to-local-keys (->> env-keys
                           (map #(vector (->> % with-maker-postfix symbol (ns-resolve ns))
                                         %))
                           ;vars
                           (filter first)
                           (into {}))
        goal-var (or (->> goal-symbol with-maker-postfix symbol (ns-resolve ns))
                     (throw (ex-info (str "Undefined goal: " goal-symbol) {})))]
    (graph/topsort-component (partial dependencies-in-ctx to-local-keys)
                             goal-var)))

(defn make-with
  "Make a goal out of the environment"
  [execution-strategy context-ns goal-sym env]
  (let [goal-map (goal-sym-goal-map context-ns context-ns goal-sym)
        goal-vars-sorted (sorted-goal-vars context-ns goal-sym (-> env
                                                                   keys
                                                                   set))
        in-ctx? (or env {})
        sorted-goal-maps (map (partial goal-var-goal-map context-ns) goal-vars-sorted)
        local-defs (->> sorted-goal-maps
                        (mapcat (juxt :goal-local
                                      #(goal-maker-call execution-strategy
                                                        %
                                                        (->> (goal-map-dep-goal-maps context-ns %)
                                                             (map (fn [p] (assoc p :in-ctx (-> p :goal-local in-ctx?))))))))
                        (into []))
        ;FIXME too implicit and needlessly limited
        undefined-goals (remove (comp :arglists :goal-meta) sorted-goal-maps)]
    (when (seq undefined-goals)
      (throw (ex-info (str "Undefined goals: " (string/join ", " (map :goal-local undefined-goals)))
                      {:goals (mapv :goal-local undefined-goals)
                       :local-defs (->> undefined-goals
                                        (partition-all 2)
                                        (map vec))
                       :for goal-map})))
    `(let [~@(->> local-defs
                  (drop-last 2))]
       ~(->> local-defs
             (take-last 2)
             second))))

(defmacro make
  [goal]
  (make-with ::sequential *ns* goal &env))

(defmacro make<>
  [goal]
  (make-with ::async *ns* goal &env))


(defmacro with-config
  "Makes the first parameter as a configuration goal at compile time to extract the keys.
  At runtime it checks if the same keys are present."
  [configs & body]
  ;is it less readable if it is (more) hygenic? is it better?
  ;TODO ^^^
  (let [[compile-time-config-form config-form] (if (vector? configs)
                                                 configs
                                                 [configs configs])
        config-keys (keys (eval compile-time-config-form))
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

(defn- args-map
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

(defn- rebuild-args
  [doc params body]
  (-> (remove nil? [doc params])
      (concat body)))

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
       (remove #(identical? *ns* (-> % :goal-meta :ns)))
       (map #(let [the-name (-> % :goal-meta :name)]
               (list 'refer `(quote ~(-> % :goal-meta :ns ns-name))
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
        deps-goal-maps (goal-map-dep-goal-maps *ns* base-goal-map)
        additional-param-goal-maps (remove (set param-goal-maps) deps-goal-maps)]
    `(do
       ~@(build-refers-for additional-param-goal-maps)
       (defgoal ~name
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

(defmacro defgoal<>
  [name & fdecl]
  (apply list 'defgoal (vary-meta name merge {::goal-type ::async-goal-channel})
         fdecl))

(defmacro defgoal<-
  [name & fdecl]
  (let [{:keys [doc params body]} (args-map [[:doc string?]
                                             [:params vector?]]
                                            fdecl)
        callback-body `[(let [result# (clojure.core.async/promise-chan)
                              ~'yield (fn yield-fn# [v#] (clojure.core.async/put! result# v#))]
                          ~@body
                          result#)]]
    `(defgoal<> ~name
       ~@(rebuild-args doc params callback-body))))

(defn throw??
  "Throw throwable, return param otherwise."
  [in]
  (if (instance? Throwable in)
    (throw (ex-info "Value is Throwable" {} in))
    in))

(defn valid??
  [sth]
  (cond
    (nil? sth)
    (throw (ex-info "Nil is invalid." {}))

    (instance? Throwable sth)
    (throw (ex-info "Value is Throwable" {} sth))

    :default
    sth))

(defn take??
  [ch]
  (valid?? (a/<!! ch)))

(defn take-in??
  ([ch msec]
   (valid?? (a/alt!! ch ([v] v)
                     (a/timeout msec) ([] (ex-info "Timed out" {:ch (str ch)}))))))

(def cases-map-atom (atom {}))

(defmethod goal-maker-call [::sequential ::case]
  [_ goal-map goal-deps]
  (let [cases (@cases-map-atom goal-map)]
    `(case (~(goal-maker-symbol goal-map)
             ~@(map :goal-local goal-deps))
       ~@(mapcat (fn [[case-item case-goal-map]]
                   (list case-item `(make ~(goal-map-goal-ns-symbol case-goal-map)))) cases))))

;TODO
#_(defmethod goal-maker-call [::async ::case]
    [_ goal-map goal-deps]
    )

(defmacro register-case
  ([dispatcher dispatch-value case-goal]
   `(swap! cases-map-atom assoc-in [(goal-sym-goal-map *ns* *ns* '~dispatcher)
                                    ~dispatch-value]
           ;FIXME ns ns?
           (goal-sym-goal-map *ns* *ns* '~case-goal)))
  ([dispatcher case-goal]
   `(register-case ~dispatcher ~case-goal ~case-goal)))
