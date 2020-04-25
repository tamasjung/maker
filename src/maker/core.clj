(ns maker.core
  (:require [maker.graph :as graph]
            [clojure.string :as string]
            [clojure.core.async :as a])
  (:import (java.util.concurrent Executors)
           (java.util WeakHashMap
                      Collections)))

(defn inj-munge
  "Injective munge"
  [s]
  (-> s
      (string/replace "+" "++")
      (string/replace "!" "+!")
      (string/replace "/" "!")
      (string/replace "_" "+_")
      (string/replace "." "_")))

(def async-callbacks ['yield])

(defn with-maker-postfix
  [s]
  (str s \*))

(defn without-maker-postfix
  [s]
  (when (re-find #"\*$" s)
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

(defn resolve-in
  [sym ns]
  (or (some
        #(some-> ns % (get sym))
        [ns-publics ns-refers])
      (resolve sym)))

(defn goal-maker-symbol
  "Calculates the goal maker fn's symbol"
  [goal-map]
  (let [{:keys [ns name]} (:goal-meta goal-map)]
    (when name
      (->> [ns name]
           (string/join "/")
           symbol))))

(defn goal-symbol
  "Calculates the goal's namespace qualified symbol"
  [{:keys [goal-local] :as goal-map}]
  (let [{:keys [ns]} (:goal-meta goal-map)]
    (when goal-local
      (->> [ns goal-local]
           (string/join "/")
           symbol))))


(def ^:dynamic *maker-ns* nil)

(defn goal-param-goal-local
  [ns name]
  (let [context-ns (or *maker-ns* *ns*)]
    (if (= context-ns ns)
      (if (symbol? name)
        name
        (symbol name))
      (-> [ns name]
          (->> (string/join "/"))
          inj-munge
          symbol))))

(defn goal-map-from-goal-var
  [goal-var]
  {:goal-var goal-var
   :goal-local (goal-param-goal-local
                 (-> goal-var meta :ns)
                 (-> goal-var meta :name str without-maker-postfix))
   :goal-meta (meta goal-var)})

(defn goal-param-goal-map
  [ns goal-param]
  (when (and goal-param
             (not= goal-param '?))
    (let [whole-p (whole-param goal-param)
          referred-goal-name (with-maker-postfix whole-p)
          goal-var (-> referred-goal-name
                       symbol
                       (resolve-in ns))]
      (if-not goal-var
        (throw (ex-info (str "Unknown goal " goal-param)
                        {:ns (ns-name ns)
                         :goal-param goal-param}))
        (goal-map-from-goal-var goal-var)))))

(defn goal-map-dep-goal-maps
  "Reads the deps from the goal's meta"
  [goal-map]
  (let [g-meta (:goal-meta goal-map)
        ns (-> g-meta :ns)]
    (->> g-meta
         :arglists
         first
         (#(if (-> goal-map :goal-meta ::goal-type (= ::async-goal-callback))
             (drop (count async-callbacks) %)
             %))
         (map (partial goal-param-goal-map ns)))))

(defn create-maker-state
  [env]
  {:bindings {}
   :local-env (or env #{})
   :circular-dep #{}
   :walk-goal-list []
   :rev-dep-goals {}})

(defn combine-items
  [m1 m2]
  (reduce-kv
    (fn [acc k v]
      (update acc k (comp distinct (fnil into [])) v))
    m1
    m2))

(defn combine-maker-state
  [new-state old-state]
  (reduce (fn _cmsr [acc [k comb-fn default]]
            (if-let [new-val (k new-state)]
              (update acc k (fnil comb-fn default) new-val)
              acc))
          old-state
          [[:bindings merge {}]
           [:rev-dep-goals combine-items {}]
           [:local-env into #{}]
           [:circular-dep into #{}]
           [:walk-goal-list (comp distinct concat) []]]))

(declare handle-goal)

(defn run-on-goals
  [state goals]
  (if-let [goal (first goals)]
    (if (-> state :local-env (get (:goal-local goal)))
      (recur state (rest goals))
      (if (-> state :circular-dep (get goal))
        (throw (ex-info
                 "Circular dependency:"
                 {:goal goal
                  :walk-goal-list (:walk-goal-list state)}))
        (update (run-on-goals
                  (handle-goal
                    goal
                    (update state :circular-dep conj goal))
                  (rest goals))
                :circular-dep disj goal)))
    state))

(defmulti goal-maker-call (fn [goal-map _]
                            (-> goal-map
                                :goal-meta
                                ::goal-type)))

(defmethod goal-maker-call ::async-goal-callback
  [goal-map goal-deps]
  `(let [result# (promise)]
     (~(goal-maker-symbol goal-map)
       #(deliver result# %)
       ~@(->> goal-deps
              (map :goal-local)))
     @result#))

(defmethod goal-maker-call ::async-goal-channel
  [goal-map goal-deps]
  `(clojure.core.async/<!! (~(goal-maker-symbol goal-map)
                             ~@(map :goal-local goal-deps))))

(defmethod goal-maker-call :default
  [goal-map goal-deps]
  `(~(goal-maker-symbol goal-map)
     ~@(map :goal-local goal-deps)))

(defn handle-goal
  [goal in-state]
  (let [dep-goals (goal-map-dep-goal-maps goal)
        dependencies-state (run-on-goals in-state dep-goals)]
    (combine-maker-state
      dependencies-state
      {:bindings {goal [(:goal-local goal) (goal-maker-call goal
                                                            dep-goals)]}
       :rev-dep-goals (->> dep-goals
                           (map #(vector % [goal]))
                           (into {}))
       :walk-goal-list [goal]
       :local-env #{(:goal-local goal)}})))

(defn make-internal
  [{:keys [walk-goal-list bindings] :as _state} goal]
  (let [local-defs (->> walk-goal-list
                        reverse
                        (map bindings)
                        (reduce into []))
        only-decl (filter (comp not :arglists :goal-meta) walk-goal-list)]
    (when (seq only-decl)
      (throw (ex-info "Undefined goals" {:goals (mapv :goal-local only-decl)
                                         :local-defs (->> only-decl
                                                          (partition-all 2)
                                                          (map vec))
                                         :for goal})))
    `(let [~@(->> local-defs
                  (drop-last 2))]
       ~(->> local-defs
             (take-last 2)
             second))))

(defn rev-deps-set
  [{:keys [rev-dep-goals]} goal-list]
  (->> goal-list
       (mapcat (graph/dependents rev-dep-goals))
       (into goal-list)
       set))

(defn local-rev-deps
  [state env ns]
  (let [local-goals (->> env
                         keys
                         (map (partial goal-param-goal-map ns)))]
    (rev-deps-set state local-goals)))

(defn discover-dependencies
  [env-keys-set goal]
  (-> env-keys-set
      create-maker-state
      (run-on-goals [goal])))

(defmacro make-with
  "Make a goal out of the environment"
  [goal-sym env]
  (let [goal (goal-param-goal-map *ns* goal-sym)
        end-state (discover-dependencies (-> env
                                             keys
                                             set) goal)]
    (make-internal end-state goal)))

(defmacro make
  [goal]
  `(make-with ~goal ~&env))

(defn maker-fn-name-with-goal-meta
  [name]
  (with-meta (-> name
                 with-maker-postfix
                 symbol)
             (meta name)))

(defn- structured-args
  [definitions args-in]
  (loop [rest-defintions definitions
         structured-args {}
         args args-in]
    (if-not (seq rest-defintions)
      (assoc structured-args :body (seq args))
      (let [[next-arg & next-args] args]
        (if-not next-arg
          (assoc structured-args :body nil)
          (let [[_skipped-defs [definition & remaning-definitions]]
                (split-with #((complement (second %)) next-arg) rest-defintions)]
            (if-not definition
              (assoc structured-args :body (seq args))
              (recur remaning-definitions
                     (assoc structured-args (first definition) next-arg)
                     next-args))))))))

(defn- rebuild-args
  [doc params body]
  (-> (remove nil? [doc params])
      (concat body)))
#_
(prn (:body (structured-args [[:doc string?]
                              [:params vector?]]
                             '(
                                "Just another goal but now using the defgoal - the same effect."
                                [other]
                                (str other "-another")))))

(defn doc-and-params
  [f [first-param second-param :as fdecl]]
  (if (string? first-param)
    (apply list first-param (f second-param) (nnext fdecl))
    (apply list (f first-param) (rest fdecl))))

(defmacro defgoal
  [name & fdecl]
  (let [{:keys [doc params body]} (structured-args [[:doc string?]
                                                    [:params vector?]]
                                                   fdecl)]
    `(defn ~(maker-fn-name-with-goal-meta name)
       ~@(rebuild-args doc params body))))

(defmacro defgoalfn                                         ;better name? dash or not dash
  [name & args]
  (let [{:keys [doc params body goal-sym]} (structured-args [[:doc string?]
                                                             [:params vector?]
                                                             [:goal-sym symbol?]]
                                                            args)
        param-goal-maps (map (partial goal-param-goal-map *ns*) params)
        base-goal-map (goal-param-goal-map *ns* goal-sym)
        deps-goal-maps (goal-map-dep-goal-maps base-goal-map)
        additional-param-goal-maps (remove (set param-goal-maps) deps-goal-maps)]
    `(do
       ~@(->> additional-param-goal-maps
              (remove #(identical? *ns* (-> % :goal-meta :ns)))
              (map #(let [the-name (-> % :goal-meta :name)]
                      (list 'refer `(quote ~(-> % :goal-meta :ns ns-name))
                            :only `(quote [~the-name])
                            :rename `(quote ~{the-name (-> % :goal-local with-maker-postfix symbol)})))))
       (defgoal ~name
         ~@(concat
             (rebuild-args doc (->> additional-param-goal-maps
                                    (mapv :goal-local))
                           body)
             [`(fn ~params
                 (~(-> base-goal-map :goal-meta :name)
                   ~@(map :goal-local deps-goal-maps)))])))))

(defmacro defgoal?
  [name]
  (list 'declare (maker-fn-name-with-goal-meta name)))

(defmacro defgoal<-
  [name & fdecl]
  (do
    (apply list 'defgoal (vary-meta name merge {::goal-type ::async-goal-callback})
           (doc-and-params #(into async-callbacks %)
                           fdecl))))

(defmacro defgoal<>
  [name & fdecl]
  (do
    (apply list 'defgoal (vary-meta name merge {::goal-type ::async-goal-channel})
           (doc-and-params identity
                           fdecl))))

(declare execute-goal)

(defn initial-async-state
  [goal-map]
  (let [deps (goal-map-dep-goal-maps goal-map)]
    (when-not (< (count deps) 63)
      (throw (ex-info "The number of dependencies has to be less than 63"
                      {:goal-map goal-map})))
    {:ready-bits (->> deps
                      count
                      (bit-shift-left 1)
                      dec)
     :dep-index (->> deps
                     (map-indexed (comp vec reverse vector))
                     (into {}))
     :dep-values (-> deps count (repeat nil) vec)}))

(defn add-dep-value
  [{:keys [dep-index ready-bits] :as async-state} goal-map goal-val]
  (let [idx (get dep-index goal-map)]
    (if-not (bit-test ready-bits idx)
      (throw (ex-info "Goal value received twice" {:goal-map goal-map}))
      (-> async-state
          (update :ready-bits bit-clear idx)
          (assoc-in [:dep-values idx] goal-val)))))

(defn exec-if-ready
  [ctx-agent goal-map result]
  (if (-> result :ready-bits (= 0))
    (do
      (execute-goal ctx-agent goal-map result)
      nil)
    result))

(defn set-result
  [ctx-agent v]
  (let [result-ch (-> @ctx-agent :result second)]
    (when (a/put! result-ch v)
      (do (send ctx-agent assoc-in [:result 0] true)
          true))))

(defn receive-goal-error
  [ctx-agent err]
  (send ctx-agent update-in [:errors] conj err)
  (set-result ctx-agent err))

(defn receive-goal-value
  [ctx-agent goal-map goal-val]
  (if (instance? Throwable goal-val)
    (receive-goal-error ctx-agent goal-val)
    (send ctx-agent
          (fn [ctx]
            (assoc ctx :results
                       (->> goal-map
                            (get (-> ctx
                                     :graph
                                     :rev-dep-goals))
                            (reduce (fn [results-acc dep-goal-map]
                                      (update results-acc
                                              dep-goal-map
                                              (comp (partial exec-if-ready
                                                             ctx-agent
                                                             dep-goal-map)
                                                    add-dep-value)
                                              goal-map
                                              goal-val))
                                    (:results ctx))))))))

(defn put-to-result
  [ctx-agent v]
  (when-not (set-result ctx-agent v)
    (throw (ex-info "Result channel is closed"
                    {:ctx @ctx-agent}))))

(defn has-result
  [ctx]
  (-> ctx
      :result
      first))

(def ^:dynamic *executor* (-> (.. Runtime getRuntime availableProcessors)
                              (+ 2)
                              (Executors/newFixedThreadPool)))

(defn execute-goal
  [ctx-agent goal-map result]
  (.execute *executor*
            (bound-fn _goal-executor-fn []                  ;TBD do we want/need bound-fn later without *maker-ns*?
              (try
                (when-not (has-result @ctx-agent)

                  (let [deps (:dep-values result)
                        yield-fn (if (-> @ctx-agent :goal-map (= goal-map))
                                   (partial put-to-result ctx-agent)
                                   (partial receive-goal-value
                                            ctx-agent
                                            goal-map))]

                    (case (-> goal-map :goal-meta ::goal-type)

                      ::async-goal-channel
                      (a/go (-> goal-map
                                :goal-var
                                (apply deps)
                                (a/<!)
                                yield-fn))

                      ::async-goal-callback
                      (apply (:goal-var goal-map) (into [yield-fn] deps))

                      (yield-fn (apply (:goal-var goal-map) deps)))))
                (catch Throwable th
                  (receive-goal-error ctx-agent th))))))

(defn filter-used-goals
  [graph goal-local-pred]
  (->> graph
       :walk-goal-list
       (mapcat goal-map-dep-goal-maps)
       (filter (comp goal-local-pred :goal-local))
       distinct
       (remove (comp (set async-callbacks)
                     :goal-local))
       (map (juxt :goal-local identity))
       (into {})))

(defn create-context-for
  [ns-sym goal-param env-keys-set]
  (let [ns (find-ns ns-sym)
        goal-map (goal-param-goal-map ns goal-param)
        graph (discover-dependencies env-keys-set goal-map)
        used-from-env (filter-used-goals graph env-keys-set)
        starters (->> graph
                      :walk-goal-list
                      (filter #(when-let [arglists (-> %
                                                       :goal-meta
                                                       :arglists)]
                                 (and (= 1 (count arglists))
                                      (or
                                        (-> arglists first empty?)
                                        (and (= ::async-goal-callback
                                                (-> %
                                                    :goal-meta
                                                    ::goal-type))
                                             (-> arglists first count (= 1))))))))]
    (when-let [undefineds (->> graph
                               :walk-goal-list
                               (remove (->> used-from-env
                                            vals
                                            set))
                               (remove (comp :arglists :goal-meta))
                               (map (juxt :goal-local
                                          :goal-meta))
                               seq)]
      (throw (ex-info "Undefined goals"
                      {:undefineds undefineds})))
    {:ns ns
     :graph graph
     :starters starters
     :used-from-env used-from-env
     :goal-map goal-map
     :results (->> (:walk-goal-list graph)
                   (into (vals used-from-env))
                   (map (juxt identity initial-async-state))
                   (into {}))}))

(def starter-result {:dep-values []})

(def ctxs (-> (WeakHashMap.) (Collections/newSetFromMap)))

(defn run-make<>
  [goal-param ns-sym env-bindings]
  (binding [*maker-ns* (find-ns ns-sym)]                    ;TODO eliminate binding
    (let [result (a/chan 1)
          ctx-agent (agent (assoc (create-context-for ns-sym
                                                      goal-param
                                                      (->> env-bindings
                                                           (map first)
                                                           set))
                             :result [false result])
                           :error-handler put-to-result)
          used-from-env (:used-from-env @ctx-agent)]
      (.add ctxs ctx-agent)
      (doseq [goal-map (:starters @ctx-agent)]
        (execute-goal ctx-agent goal-map starter-result))
      (doseq [[local val] env-bindings]
        (receive-goal-value ctx-agent (get used-from-env local) val))
      [result ctx-agent])))

(defmacro make<>
  ([goal-param]
   (let [goal-map (goal-param-goal-map *ns* goal-param)
         graph (discover-dependencies (keys &env) goal-map)
         env (or &env {})
         used-from-env (filter-used-goals graph env)]
     `(run-make<> ~(list 'quote goal-param)
                  ~(list 'quote (ns-name *ns*))
                  ~(->> used-from-env
                        (map first)
                        (map #(vector (list 'quote %)
                                      %))
                        vec)))))

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

(def result-chan first)

(defn take??
  [[ch]]
  (valid?? (a/<!! ch)))

(defn take-in??
  ([[ch] msec]
   (valid?? (a/alt!! ch ([v] v)
                     (a/timeout msec) ([] (ex-info "Timed out" {:ch (str ch)}))))))

(defmacro with-goals
  [pairs & body]
  (assert (-> pairs count even?))
  `(let [~@(->> pairs
                (partition 2)
                (map (juxt (comp :goal-local
                                 (partial goal-param-goal-map *ns*)
                                 first)
                           second))
                (reduce into []))]
     ~@body))

(def cases-map-atom (atom {}))

(defmethod goal-maker-call ::case
  [goal-map goal-deps]
  (let [cases (@cases-map-atom goal-map)]
    `(case (~(goal-maker-symbol goal-map)
             ~@(map :goal-local goal-deps))
       ~@(mapcat (fn [[case-item case-goal-map]]
                   (list case-item `(make ~(goal-symbol case-goal-map)))) cases))))

(defmacro register-case
  ([dispatcher dispatch-value case-goal]
   `(swap! cases-map-atom assoc-in [(goal-param-goal-map *ns* '~dispatcher)
                                    ~dispatch-value]
           (goal-param-goal-map *ns* '~case-goal)))
  ([dispatcher case-goal]
   `(register-case ~dispatcher ~case-goal ~case-goal)))