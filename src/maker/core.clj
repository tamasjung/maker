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

(def maker-postfix \*)

(def async-callbacks ['yield])

(defn but-last-char
  [s]
  (subs s 0 (-> s count dec)))

(def escape-set #{\* \+})

(defn without-end
  [s end-char]
  (when (re-find (re-pattern (str (when (escape-set end-char)
                                    "\\")
                                  end-char
                                  "$"))
                 s)
    (but-last-char s)))

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

(defn local-dep-symbol
  "Returns the local bind symbol for a dependency"
  [goal-map]
  (:goal-local goal-map))

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
          refered-goal-name (-> whole-p
                                (str maker-postfix))
          goal-var (-> refered-goal-name
                       symbol
                       (resolve-in ns))]
      (if-not goal-var
        (on-the-fly-goal-decl ns whole-p)
        {:goal-var goal-var
         :goal-local (goal-param-goal-local
                       (-> goal-var meta :ns)
                       (-> goal-var meta :name str (without-end maker-postfix)))
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

(declare make-internal)

(defn goal-maker-call
  [goal-map goal-deps]
  `(~(or (goal-maker-symbol goal-map)
         (throw (ex-info "Missing goal definition" {:goal-map goal-map})))
     ~@(map local-dep-symbol goal-deps)))

(defn dependants-set
  [{:keys [rev-dep-goals]} item-goal-list]
  (letfn [(add-dependants [result dep-goal]
            (->> dep-goal
                 (get rev-dep-goals)
                 (map (partial add-dependants result))
                 (reduce into #{dep-goal})))]
    (reduce add-dependants #{} item-goal-list)))

(defn handle-goal
  [goal in-state]
  (let [dep-goals (goal-map-dep-goal-maps goal)
        dependencies-state (run-on-goals in-state dep-goals)]
    (-> dependencies-state
        (combine-maker-state
          {:bindings {goal [(local-dep-symbol goal) (goal-maker-call goal
                                                                     dep-goals)]}
           :rev-dep-goals (->> dep-goals
                               (map #(vector % [goal]))
                               (into {}))
           :walk-goal-list [goal]
           :local-env #{(:goal-local goal)}}))))

(defn make-internal
  [{:keys [walk-goal-list bindings] :as _state} goal]
  `(let [~@(->> walk-goal-list
                reverse
                (map bindings)
                (reduce into []))]
     ~(local-dep-symbol goal)))

(def ^:dynamic *non-local-deps* nil)

(defn local-dependants
  [state env ns]
  (let [local-goals (->> env
                         keys
                         (map (partial goal-param-goal-map ns))
                         (remove on-the-fly?))]
    (dependants-set state local-goals)))

(defn collect-non-local-deps!
  [env ns {:keys [rev-dep-goals] :as state} goal]
  (when *non-local-deps*
    (let [local-dependants (local-dependants state env ns)
          minimal-non-local-goal-set
          ((graph/border-fn goal-map-dep-goal-maps
                            (complement local-dependants))
            goal)]
      (set! *non-local-deps*
            (into *non-local-deps* minimal-non-local-goal-set)))))

(defn discover-dependencies
  [env goal]
  (-> env
      keys
      set
      create-maker-state
      (run-on-goals [goal])))

(defmacro make-with
  "Make a goal out of the environment"
  [goal-sym env]
  (let [goal (goal-param-goal-map *ns* goal-sym)
        end-state (discover-dependencies env goal)]
    (collect-non-local-deps! env *ns* end-state goal)
    (-> end-state
        (make-internal goal))))

(defmacro make
  [goal]
  `(make-with ~goal ~&env))

(defn with-goal-meta
  [name]
  (with-meta (-> name
                 (str maker-postfix)
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
             (params-fn (comp #(into additional-params %)
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

(defn value-of-goal
  [ctx goal-map]
  (get-in ctx [:values (:goal-local goal-map)]))

(defn goal-value-exists?
  [values goal-map]
  (contains? values (:goal-local goal-map)))

(declare execute-goal)

(defn distribute-goal-val
  [ctx-atom goal-map goal-val]
  (locking ctx-atom
    (let [ctx @ctx-atom]
      (binding [*maker-ns* (:ns ctx)]
        (doseq [goal-map-to-run
                (->> (-> ctx :graph :rev-dep-goals (get goal-map))
                     (filter #(->> %
                                   goal-map-dep-goal-maps
                                   (every? (partial goal-value-exists?
                                                    (:values ctx))))))]
          (execute-goal ctx-atom goal-map-to-run))))))

(defn put-to-result
  [ctx-atom v]
  (locking ctx-atom
    (-> @ctx-atom
        :result
        second
        (a/put! v (fn [not-closed] (if not-closed
                                     (swap! ctx-atom update-in [:result 0] not)
                                     (throw (ex-info "Result channel is closed"
                                                     {:ctx @ctx-atom}))))))))

(defn has-result
  [ctx-atom]
  (-> @ctx-atom
      :result
      first))

(defn receive-goal
  [ctx-atom goal-map goal-val]
  (if (instance? Throwable goal-val)
    (put-to-result ctx-atom goal-val)
    (do
      (swap! ctx-atom
             (fn [ctx]
               (if (and (-> ctx
                            :values
                            (contains? (:goal-local goal-map)))
                        (not ((@ctx-atom :used-from-env) (:goal-local goal-map))))
                 (throw (ex-info "yield called twice" {:goal-map goal-map}))
                 (assoc-in ctx [:values (:goal-local goal-map)] goal-val))))
      (distribute-goal-val ctx-atom goal-map goal-val))))

(defn receive-goal-error
  [ctx-atom goal err]
  (put-to-result ctx-atom err))

(def ^:dynamic *executor* (-> (.. Runtime getRuntime availableProcessors)
                              (+ 2)
                              (Executors/newFixedThreadPool)))

(defn execute-goal
  [ctx-atom goal-map]
  (binding [*maker-ns* (:ns @ctx-atom)]
    (.execute *executor*
              (bound-fn _goal-executor-fn []                ;TBD do we want/need this?
                (when-not (has-result ctx-atom)
                  (try
                    (let [deps (->> goal-map
                                    goal-map-dep-goal-maps
                                    (map (partial value-of-goal @ctx-atom)))
                          yield-fn (if (-> @ctx-atom :goal-map (= goal-map))
                                     (partial put-to-result ctx-atom)
                                     (partial receive-goal ctx-atom goal-map))]
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
                      (receive-goal-error ctx-atom goal-map th))))))))

(def contextes (atom {}))

(defn run-make<>
  [goal-param ctx-id env-bindings]
  (let [result (a/chan 1)
        ctx-atom (atom @(get @contextes ctx-id))            ;clone compile time context
        used-from-env (:used-from-env @ctx-atom)]
    (swap! ctx-atom assoc :result [false result])
    (doseq [goal-map (:starters @ctx-atom)]
      (execute-goal ctx-atom goal-map))
    (doseq [[local val] env-bindings]
      (receive-goal ctx-atom (get used-from-env local) val))
    result))

(defmacro make<>
  ([goal-param]
   (let [goal-map (goal-param-goal-map *ns* goal-param)
         graph (discover-dependencies &env goal-map)
         env (or &env {})
         used-from-env (->> graph
                            :walk-goal-list
                            (mapcat goal-map-dep-goal-maps)
                            (filter (comp env :goal-local))
                            distinct
                            (remove (comp (set async-callbacks)
                                          :goal-local))
                            (map (juxt :goal-local identity))
                            (into {}))
         starters (->> graph
                       :walk-goal-list
                       (filter #(when (contains? (-> % :goal-meta) :arglists)
                                  (let [arglists (-> % :goal-meta :arglists)]
                                    (and (= 1 (count arglists))
                                         (-> arglists first empty?))))))
         ctx-id (gensym goal-param)]
     (swap! contextes assoc ctx-id
            (atom {:ns *ns*
                   :graph graph
                   :starters starters
                   :used-from-env used-from-env
                   :goal-map goal-map}))
     `(run-make<> ~(list 'quote goal-param)
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
                (map (juxt (comp local-dep-symbol
                                 (partial goal-param-goal-map *ns*)
                                 first)
                           second))
                (reduce into []))]
     ~@body))