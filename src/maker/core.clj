(ns maker.core
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :as walk]))

(defn inj-munge
  "Injective munge"
  [s]
  (-> s
      (string/replace "+" "++")
      (string/replace "!" "+!")
      (string/replace "/" "!")
      (string/replace "_" "+_")
      (string/replace "." "_")))

(defn whole-param
  "Returns the ':as' symbol or itself"
  [dep]
  (cond
    (symbol? dep) dep
    (map? dep) (:as dep)
    (vector? dep) (let [[as whole] (take-last 2 dep)]
                    (when (and (= :as as)
                               (symbol? whole))
                      whole))
    :default (throw (IllegalArgumentException.
                      (str "Unrecogonized dependency:" dep ".")))))

(defn var-meta
  "Returns the meta of the var of the symbol."
  [sym]
  (let [r (eval `(-> ~sym meta))]
    r))

(defn resolve-in
  [symbol ns]
  (some
    #(some-> ns % (get symbol))                             ;;FIXME some-> ?????
    [ns-publics ns-refers]))

(defn goal-maker-symbol
  "Calculates the goal maker fn's symbol"
  [goal]
  (->> goal :goal-meta ((juxt :ns :name)) (string/join "/") symbol))

(defn local-dep-symbol
  "Returns the local bind symbol for a dependency"
  [goal]
  (:local goal))

(defn goal-for-param
  [ns param]
  (when param
    (let [whole-p (whole-param param)
          refered-goal-name (-> whole-p
                                (str "*"))
          refered-goal (-> refered-goal-name
                           symbol
                           (resolve-in ns))
          ]
      (if-not refered-goal
        (throw (IllegalArgumentException.
                 (str "Could not resolve " refered-goal-name)))
        {:goal refered-goal
         :local (if (= *ns* ns)
                  whole-p
                  (let [goal-name (-> refered-goal
                                      meta
                                      ((juxt :ns :name))
                                      (->> (string/join "/"))
                                      inj-munge)]
                    (-> (subs goal-name 0 (-> goal-name
                                              count
                                              dec))
                        symbol)))
         :goal-meta (var-meta refered-goal)}))))

(defn goal-dep-goals
  "Reads the deps from the goal's meta"
  [goal]
  (let [g-meta (:goal-meta goal)
        ns (-> g-meta :ns)]
    (->> g-meta
         :arglists
         first
         (map (partial goal-for-param ns)))))

(defn related-goal
  [ref-goal key-fn]
  (->> ref-goal :goal-meta key-fn
       (goal-for-param (-> ref-goal :goal-meta :ns))))

(defn collected-goal
  [goal]
  (related-goal goal :collect))

(defn iteration-goal
  [goal]
  (related-goal goal :for))

(defn item-goal
  [goal]
  (related-goal goal :item))

(defn multi-goal-meta
  [goal]
  (let [{:keys [selector cases] :as goal-meta}
        (-> goal :goal-meta)]
    (when (and selector cases)
      goal-meta)))

(defn create-maker-state
  [env]
  {:bindings {}
   :local-env (or env #{})
   :no-circular-dep #{}
   :walk-goal-list []
   :rev-dep-goals {}
   :item-goal-list []})

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
           [:no-circular-dep into #{}]
           [:walk-goal-list (comp distinct concat) []]
           [:item-goal-list (comp distinct concat) []]
           [:log-fn #(or %1 %2) nil]]))

(defn handler-selector
  [goal _]
  (cond
    (let [{:keys [for collect item]}
          (-> goal :goal-meta)]
      (and for collect item)) :iterator-collector
    (multi-goal-meta goal) :multi
    :else :default))

(defmulti handle-goal handler-selector)

(defn run-on-goals
  [state goals]
  (let [log-fn (or (:log-fn state)
                   (constantly nil))]
    (if-let [goal (some-> goals first)]                     ;;TODO check throughtout the ns for destructuring forms remain
      (if (-> state :local-env (get (:local goal)))
        (recur state (rest goals))
        (if (-> state :no-circular-dep (get goal))
          (throw (IllegalStateException.
                   (str "Circural dependency:"
                        goal
                        ", walk-path:"
                        (:walk-goal-list state))))
          (do
            (log-fn goal "==>" state)
            (let [res
                  (update
                    (run-on-goals
                      (handle-goal
                        goal
                        (update state :no-circular-dep conj goal))
                      (rest goals))
                    :no-circular-dep disj goal)]
              (log-fn goal "<==" res)
              res))))
      state)))

(declare make-internal)

(defn collector-maker-call
  [goal {:keys [item-goal-list] :as state}]
  (assert (= 1 (count item-goal-list)))
  (let [collected (collected-goal goal)]
    `(for [~(->> item-goal-list first local-dep-symbol)
           ~(-> goal iteration-goal local-dep-symbol)]
       ~(make-internal state collected false))))

(defn goal-maker-call
  [goal goal-deps]
  `(~(goal-maker-symbol goal) ~@(map local-dep-symbol goal-deps)))

(defn multi-maker-call
  [goal cases-states case-goals]
  (let [{:keys [selector]} (multi-goal-meta goal)]
    `(case ~selector
       ~@(mapcat
           #(vector (local-dep-symbol %)
                    (make-internal (get cases-states %)
                                   %
                                   false))
           case-goals))))

(defn dependants
  [{:keys [rev-dep-goals item-goal-list]}]
  (letfn [(add-dependants [result dep]
            (->> dep
                 rev-dep-goals
                 (map (partial add-dependants result))
                 (reduce into #{dep})))]
    (reduce add-dependants #{} item-goal-list)))

(defmethod handle-goal :iterator-collector
  [goal in-state]
  (let [item-goal (item-goal goal)
        new-item-list [item-goal]
        up-state (-> in-state
                     (run-on-goals [(iteration-goal goal)])
                     (combine-maker-state
                       {:item-goal-list new-item-list
                        :local-env #{(:local item-goal)}}))
        stored-keys [:item-goal-list :walk-goal-list]
        collected-state (run-on-goals
                          (-> up-state
                              (merge (select-keys stored-keys
                                                  (create-maker-state nil))))
                          [(collected-goal goal)])
        local-dependants (dependants (assoc collected-state
                                       :item-goal-list new-item-list))
        collector-maker (collector-maker-call
                          goal
                          (update collected-state
                                  :walk-goal-list #(filter local-dependants %)))
        dep-goals (map iteration-goal new-item-list)
        result-state
        (-> collected-state
            (assoc :local-env (:local-env up-state))
            (merge (select-keys up-state                    ;;restore state
                                stored-keys))
            (update :walk-goal-list concat
                    (remove local-dependants
                            (:walk-goal-list collected-state)))
            (update :local-env into (set/difference (:local-env collected-state)
                                                    local-dependants)))]
    (combine-maker-state
      result-state
      {:bindings {goal [(local-dep-symbol goal) collector-maker]}
       :walk-goal-list [goal]
       :rev-dep-goals (->> dep-goals
                           (map #(vector % [goal]))
                           (into {}))
       :local-env #{(:local goal)}})))

(defmethod handle-goal :multi
  [goal in-state]
  (let [{:keys [selector cases] :as multi-meta} (multi-goal-meta goal)
        goal-ns (-> goal :goal-meta :ns)
        selector-goal (goal-for-param goal-ns selector)
        case-goals (map (partial goal-for-param goal-ns) cases)
        selector-state (run-on-goals in-state [selector-goal])
        cases-states (->> case-goals
                          (map #(vector % (run-on-goals
                                            (assoc selector-state
                                              :walk-goal-list [])
                                            [%])))
                          (into {}))

        cases-states-list (map #(get cases-states %) case-goals)
        combined-cases-state
        (reduce combine-maker-state selector-state cases-states-list)

        common-set (->> cases-states-list
                        (mapv (comp set :walk-goal-list))
                        (reduce set/intersection))
        common-walk-list (->> combined-cases-state
                              :walk-goal-list
                              (filter common-set))]
    (-> selector-state
        (combine-maker-state (-> combined-cases-state
                                 (assoc :walk-goal-list common-walk-list)))
        (combine-maker-state
          {:bindings {goal [(local-dep-symbol goal)
                            (multi-maker-call goal
                                              (reduce-kv
                                                (fn [acc k v]
                                                  (assoc acc
                                                    k
                                                    (update v
                                                            :walk-goal-list
                                                            (partial
                                                              remove
                                                              common-set))))
                                                {}
                                                cases-states)
                                              case-goals)]}
           :rev-dep-goals (->> (conj case-goals selector-goal)
                               (map #(vector % [goal]))
                               (into {}))
           :walk-goal-list [goal]
           :local-env #{(:local goal)}}))))

(defmethod handle-goal :default
  [goal in-state]
  (let [dep-goals (goal-dep-goals goal)
        dependencies-state (run-on-goals in-state dep-goals)]
    (-> dependencies-state
        (combine-maker-state
          {:bindings {goal [(local-dep-symbol goal) (goal-maker-call goal
                                                                     dep-goals)]}
           :rev-dep-goals (->> dep-goals
                               (map #(vector % [goal]))
                               (into {}))
           :walk-goal-list [goal]
           :local-env #{(:local goal)}}))))

(defn load-depencies
  "Call 'require' on every given namespace if necessary"
  [namespaces]
  #_(doseq [r namespaces]
      (try
        (ns-name r)                                         ;; TODO any better way to detect an unloaded ns?
        (catch Throwable _
          (require r)))))

(defn make-internal
  [{:keys [walk-goal-list bindings] :as state} goal fail-on-opens]
  `(let [~@(->> walk-goal-list
                reverse
                (map bindings)
                (reduce into []))]
     ~(local-dep-symbol goal)))

(defn print-generated-code
  [form]
  (->> form
       (walk/postwalk
         (fn [s]
           (cond
             (symbol? s)
             (-> s
                 str
                 (string/replace #"^clojure.core/" "")
                 symbol)

             :default s)))
       pprint)
  form)

(defmacro make-with
  "Make a goal out of the environment"
  [goal-sym env log-fn]
  (let [state (-> env keys set create-maker-state (assoc :log-fn log-fn))
        goal (goal-for-param *ns* goal-sym)]
    (-> (run-on-goals state [goal])
        (make-internal goal true)
        (cond->
          log-fn print-generated-code))))

(defmacro make
  [goal]
  `(make-with ~goal ~&env nil))

(def #^{:macro true} *- #'make)

(defn log-fn
  [& args]
  (pprint (->> args
               (map #(if (map? %)
                      (dissoc % :log-fn :bindings)
                      %))
               vec)))

(defmacro prn-make
  [goal & [user-log-fn]]
  `(make-with ~goal ~&env ~(or (and user-log-fn
                                    (eval user-log-fn))
                               log-fn)))

(def #^{:macro true} pr*- #'prn-make)

(defmacro with
  [pairs & body]
  (assert (-> pairs count even?))
  `(let [~@(->> pairs
                (partition 2)
                (map (juxt (comp local-dep-symbol)
                           second))
                (reduce into []))]
     ~@body))

;(defmacro defgoal
;  [the-name deps & body]
;  `(do
;     (defn ~the-name [~@(map dep-param-symbol deps)]
;       ~@body)
;     (alter-meta! (var ~the-name)
;                  assoc
;                  :goal true
;                  :deps (quote ~(mapv (comp alias->fqn
;                                            goal-maker-symbol)
;                                      deps)))))
;
;(defmacro declare-goal
;  "Declare a goal"
;  [the-name]
;  `(defgoal ~the-name
;            []
;            (throw (ex-info ~(str "Goal definition is missing "
;                                  *ns* "/" the-name)
;                            {:type ::goal-runtime}))))
