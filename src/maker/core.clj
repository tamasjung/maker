(ns maker.core
  (:require [maker.graph :as graph]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :as walk]
            [clojure.core.async :as a])
  (:import (java.util.concurrent Executors)))

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
  [symbol ns]
  (some
    #(some-> ns % (get symbol))
    [ns-publics ns-refers]))

(defn goal-maker-symbol
  "Calculates the goal maker fn's symbol"
  [goal-map]
  (let [{:keys [ns name]} (:goal-meta goal-map)]
    (when name
      (->> [ns name]
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

(defn on-the-fly-goal-decl
  [ns whole-param-name]
  (let [s (goal-param-goal-local ns whole-param-name)]
    {:goal-local s}))

(def on-the-fly? (complement :goal-var))

(defn goal-param-goal-map
  [ns goal-param]
  (when goal-param
    (let [whole-p (whole-param goal-param)
          referred-goal-name (-> whole-p
                                 with-maker-postfix)
          goal-var (-> referred-goal-name
                       symbol
                       (resolve-in ns))]
      (if-not goal-var
        (on-the-fly-goal-decl ns whole-p)
        {:goal-var goal-var
         :goal-local (goal-param-goal-local
                       (-> goal-var meta :ns)
                       (-> goal-var meta :name str (without-maker-postfix)))
         :goal-meta (meta goal-var)}))))

(defn goal-map-dep-goal-maps
  "Reads the deps from the goal's meta"
  [goal-map]
  (let [g-meta (:goal-meta goal-map)
        ns (-> g-meta :ns)]
    (->> g-meta
         :arglists
         first
         (#(if (-> goal-map :goal-meta ::async-goal-callback)
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

(defn goal-maker-call
  [goal-map goal-deps]
  `(~(or (goal-maker-symbol goal-map)
         (throw (ex-info "Missing goal definition" {:goal-map goal-map})))
     ~@(map :goal-local goal-deps)))

(defn handle-goal
  [goal in-state]
  (let [dep-goals (goal-map-dep-goal-maps goal)
        dependencies-state (run-on-goals in-state dep-goals)]
    (-> dependencies-state
        (combine-maker-state
          {:bindings {goal [(:goal-local goal) (goal-maker-call goal
                                                                     dep-goals)]}
           :rev-dep-goals (->> dep-goals
                               (map #(vector % [goal]))
                               (into {}))
           :walk-goal-list [goal]
           :local-env #{(:goal-local goal)}}))))

(def ^:dynamic *non-local-deps* nil)

(defn make-internal
  [{:keys [walk-goal-list bindings] :as _state} goal]
  (let [local-defs (->> walk-goal-list
                        reverse
                        (map bindings)
                        (reduce into []))
        only-decl (->> walk-goal-list
                       (filter (comp not :arglists :goal-meta)))]
    (when (and (not *non-local-deps*)
               (seq only-decl))
      (throw (ex-info "Undefined goals" {:goals (mapv :goal-local only-decl)
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
                         (map (partial goal-param-goal-map ns))
                         (remove on-the-fly?))]
    (rev-deps-set state local-goals)))

(defn collect-non-local-deps!
  [env ns {:keys [rev-dep-goals] :as state} goal]
  (when *non-local-deps*
    (let [local-rev-deps-set (local-rev-deps state env ns)
          minimal-non-local-goal-set
          ((graph/border-fn goal-map-dep-goal-maps
                            (complement local-rev-deps-set))
            goal)]
      (set! *non-local-deps*
            (into *non-local-deps* minimal-non-local-goal-set)))))

(defn discover-dependencies
  [env-keys-set goal]
  (-> env-keys-set
      create-maker-state
      (run-on-goals [goal])))

(def async-types #{::async-goal-channel
                   ::async-goal-callback})

(defmacro make-with
  "Make a goal out of the environment"
  [goal-sym env]
  (let [goal (goal-param-goal-map *ns* goal-sym)
        end-state (discover-dependencies (-> env
                                             keys
                                             set) goal)]
    (when-let [async-goal (some #(-> %
                                     :goal-meta
                                     keys
                                     set
                                     (set/intersection async-types)
                                     seq)
                                (:walk-goal-list end-state))]
      (throw (ex-info "Synchronous make depends on async goal"
                      {:goal-map async-goal})))
    (collect-non-local-deps! env *ns* end-state goal)
    (-> end-state
        (make-internal goal))))

(defmacro make
  [goal]
  `(make-with ~goal ~&env))

(defn with-goal-meta
  [name]
  (with-meta (-> name
                 with-maker-postfix
                 symbol)
             (meta name)))

(defn params-fn
  [f [first-param second-param :as fdecl]]
  (if (string? first-param)
    (apply list first-param (f second-param) (nnext fdecl))
    (apply list (f first-param) (rest fdecl))))

(defn first-param
  [fdecl]
  (if (-> fdecl first string?)
    (-> fdecl second first)
    (ffirst fdecl)))

(defmacro defgoal
  [name & fdecl]
  (if (-> fdecl first-param (= '?))
    (let [additional-params
          (binding [*non-local-deps* []]
            ;;this is really a dirty trick but what can we do?
            (eval (apply list 'defn name fdecl))
            (->> *non-local-deps*
                 (map :goal-local)
                 vec))]
      (apply list
             'defn
             (with-goal-meta name)
             (params-fn (comp vec
                              distinct
                              #(into additional-params %)
                              rest)
                        fdecl)))
    (apply list
           `defn
           (with-goal-meta name)
           (params-fn identity
                      fdecl))))

(defmacro defgoal?
  [name]
  (list 'declare (with-goal-meta name)))

(defmacro defgoal<-
  [name & fdecl]
  (do
    (apply list 'defgoal (vary-meta name merge {::async-goal-callback true})
           (params-fn #(into async-callbacks %)
                      fdecl))))

(defmacro defgoal<>
  [name & fdecl]
  (do
    (apply list 'defgoal (vary-meta name merge {::async-goal-channel true})
           (params-fn identity
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
  [{:keys [dep-index ready-bits dep-values] :as async-state} goal-map goal-val]
  (let [idx (get dep-index goal-map)]
    (if-not (bit-test ready-bits idx)
      (throw (ex-info "Goal value received twice" {:goal-map goal-map}))
      (-> async-state
          (update :ready-bits bit-clear idx)
          (assoc-in [:dep-values idx] goal-val)))))

(defn exec-if-ready
  [ctx-atom goal-map result]
  (if (-> result :ready-bits (= 0))
    (do
      (execute-goal ctx-atom goal-map result)
      nil)
    result))

(defn receive-goal-value
  [ctx-atom goal-map goal-val]
  (swap! ctx-atom update :results
         (fn [results]
           (->> goal-map
                (get (-> @ctx-atom
                         :graph
                         :rev-dep-goals))
                (reduce (fn [results-acc dep-goal-map]
                          (update results-acc
                                  dep-goal-map
                                  (comp (partial exec-if-ready
                                                 ctx-atom
                                                 dep-goal-map)
                                        add-dep-value)
                                  goal-map
                                  goal-val))
                        results)))))

(defn put-to-result
  [ctx-atom v]
  (let [result-ch (-> @ctx-atom :result second)]
    (locking ctx-atom
      (if (a/put! result-ch v)
        (swap! ctx-atom update-in [:result 0] not)
        (throw (ex-info "Result channel is closed"
                        {:ctx @ctx-atom}))))))

(defn has-result
  [ctx-atom]
  (-> @ctx-atom
      :result
      first))

(defn receive-goal-error
  [ctx-atom goal err]
  (put-to-result ctx-atom err))

(def ^:dynamic *executor* (-> (.. Runtime getRuntime availableProcessors)
                              (+ 2)
                              (Executors/newFixedThreadPool)))

(defn execute-goal
  [ctx-atom goal-map result]
  (.execute *executor*
            (bound-fn _goal-executor-fn []                  ;TBD do we want/need bound-fn later without *maker-ns*?
              (when-not (has-result ctx-atom)
                (try
                  (let [deps (:dep-values result)
                        yield-fn (if (-> @ctx-atom :goal-map (= goal-map))
                                   (partial put-to-result ctx-atom)
                                   (partial receive-goal-value
                                            ctx-atom
                                            goal-map))]
                    (let [meta (-> goal-map :goal-meta)]
                      (cond
                        (::async-goal-channel meta)
                        (a/go (-> goal-map
                                  :goal-var
                                  (apply deps)
                                  (a/<!)
                                  yield-fn))

                        (::async-goal-callback meta)
                        (apply (:goal-var goal-map) (into [yield-fn] deps))

                        :default
                        (yield-fn (apply (:goal-var goal-map) deps)))))
                  (catch Throwable th
                    (receive-goal-error ctx-atom goal-map th)))))))

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

(def contextes (atom {}))

(defn ensure-context-for
  [ns-sym context-id goal-param env-keys-set]
  (or (get @contextes context-id)
      ;double-checked locking
      (locking context-id
        (or (get @contextes context-id)
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
                                                (-> arglists first empty?)))))
                  ctx {:ns ns
                       :graph graph
                       :starters starters
                       :used-from-env used-from-env
                       :goal-map goal-map
                       :results (->> (:walk-goal-list graph)
                                     (into (vals used-from-env))
                                     (map (juxt identity initial-async-state))
                                     (into {}))}]
              (swap! contextes assoc context-id ctx)
              ctx)))))

(def starter-result {:dep-values []})

(defn run-make<>
  [goal-param ns-sym ctx-id env-bindings]
  (binding [*maker-ns* (find-ns ns-sym)]                    ;TODO eliminate binding
    (let [result (a/chan 1)
          ctx-atom (atom (ensure-context-for ns-sym
                                             ctx-id
                                             goal-param
                                             (->> env-bindings
                                                  (map first)
                                                  set)))    ;clone cached context
          used-from-env (:used-from-env @ctx-atom)]
      (swap! ctx-atom assoc :result [false result])
      (doseq [goal-map (:starters @ctx-atom)]
        (execute-goal ctx-atom goal-map starter-result))
      (doseq [[local val] env-bindings]
        (receive-goal-value ctx-atom (get used-from-env local) val))
      result)))

(defmacro make<>
  ([goal-param]
   (let [goal-map (goal-param-goal-map *ns* goal-param)
         graph (discover-dependencies (keys &env) goal-map)
         env (or &env {})
         used-from-env (filter-used-goals graph env)
         ctx-id (gensym goal-param)]
     `(run-make<> ~(list 'quote goal-param)
                  ~(list 'quote (ns-name *ns*))
                  ~(list 'quote ctx-id)
                  ~(->> used-from-env
                        (map (comp first))
                        (map #(vector (list 'quote %)
                                      %))
                        vec)))))

(defn throw??
  "Throw throwable, return param otherwise."
  [in]
  (if (instance? Throwable in)
    (throw (ex-info "Value is a Throwable" {} in))
    in))

(defn valid??
  [sth]
  (cond
    (nil? sth)
    (throw (ex-info "Nil is invalid here." {}))

    (instance? Throwable sth)
    (throw (ex-info "Value is throwable" {} sth))

    :default
    sth))

(defn take??
  [ch]
  (valid?? (a/<!! ch)))

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