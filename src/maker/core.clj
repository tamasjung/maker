(ns maker.core
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]))

(defn inj-munge
  "Injective munge"                                         ;;...it will be.
  [s]
  (-> s
      ;(string/replace "+" "++")
      ;(string/replace "!" "+!")
      (string/replace "/" "!")
      ;(string/replace "_" "+_")
      (string/replace "." "_")))

(defn inj-munge-inv
  [s]
  (-> s
      (string/replace #"(?<!\+)_" ".")
      (string/replace #"(?<!\+)!" "/")))

(defn whole-symbol
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

(defn split-fqn
  "Split fully-quelified name into parts"
  [fqn]
  (-> fqn str (string/split #"/")))

(defn symbol->meta
  "Returns the meta of the var of the symbol."
  [sym]
  (eval `(-> ~sym var meta)))

(defn alias->fqn
  "Convert an alias to fully-qualified symbol"
  [sym]
  (->> sym
       symbol->meta
       ((juxt (comp :name bean :ns) :name))
       (string/join "/")
       symbol))

(defn dep-param-symbol
  "Returns the parameter form."
  [dep]
  (whole-symbol dep))

(defn goal-maker-symbol
  "Calculates the goal maker fn's symbol"
  [dep]
  (-> dep whole-symbol (str "*") symbol))

(defn goal-meta
  [goal]
  (-> goal
      whole-symbol
      goal-maker-symbol
      symbol->meta))

(defn local-dep-symbol
  "Returns the local bind symbol for a dependency"
  [dep]
  (-> dep whole-symbol goal-maker-symbol
      alias->fqn split-fqn second butlast                   ;; cut the *
      (->> (apply str)) symbol))

(defn goal-deps
  "Reads the deps from the goal's meta"
  [goal]
  (->> goal
       goal-meta
       :arglists
       last))

(defn collected-dep
  [goal]
  (-> goal goal-meta :collect))

(defn iteration-dep
  [goal]
  (-> goal goal-meta :for))

(defn item-dep
  [goal]
  (-> goal goal-meta :item))

(defn multi-dep
  [goal]
  (let [{:keys [selector cases] :as goal-meta}
        (-> goal whole-symbol goal-maker-symbol symbol->meta)]
    (when (and selector cases)
      goal-meta)))

;(defn goal->namespace
;  "Returns the namespace of the goal symbol"
;  [goal]
;  (-> goal
;      goal-maker-symbol
;      alias->fqn
;      split-fqn
;      first
;      symbol))
;
;(defn goal->name
;  "Returns the name (without ns) of the goal as symbol"
;  [goal]
;  (-> goal
;      goal-maker-symbol
;      alias->fqn
;      split-fqn
;      second
;      symbol))

(defn create-maker-state
  [env]
  {:bindings {}
   :env (or env #{})
   :no-circular-dep #{}
   :walk-list []
   :rev-deps {}
   :item-list []})

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
           [:rev-deps combine-items {}]
           [:env into #{}]
           [:no-circular-dep into #{}]
           [:walk-list (comp distinct concat) []]
           [:item-list (comp distinct concat) []]
           [:log-fn #(or %1 %2) nil]]))

(defn reverse-dependencies
  [dep]
  (->> dep
       goal-deps
       (map whole-symbol)
       (map #(vector % [dep]))
       (into {})))

(defn handler-selector
  [goal _]
  (cond
    (and (iteration-dep goal)
         (collected-dep goal)) :iterator-collector
    (iteration-dep goal) :iteration
    (collected-dep goal) :collector
    (multi-dep goal) :multi
    :else :default))

(defmulti handle-goal handler-selector)

(defn run-on-deps
  [state deps]
  (let [log-fn (or (:log-fn state)
                   (constantly nil))]
    (if-let [dep (some-> deps first local-dep-symbol)]      ;;TODO check throughtout the ns for destructuring forms remain
      (if (-> state :env dep)
        (recur state (rest deps))
        (if (-> state :no-circular-dep (get dep))
          (throw (IllegalStateException.
                   (str "Circural dependency:"
                        dep
                        ", walk-path:"
                        (:walk-list state))))
          (do
            (log-fn dep "==>" state)
            (let [res
                  (update
                    (run-on-deps
                      (handle-goal
                        dep
                        (update state :no-circular-dep conj dep))
                      (rest deps))
                    :no-circular-dep disj dep)]
              (log-fn dep "<==" res)
              res))))
      state)))

(defmethod handle-goal :iteration
  [goal old-state]
  (combine-maker-state
    {:item-list [goal]
     :env #{goal}}
    old-state))

(declare make-internal)

(defn collector-maker-call
  [goal {:keys [item-list] :as state}]
  (let [collected (collected-dep goal)]
    `(for [~@(->> item-list
                  (map (juxt local-dep-symbol
                             iteration-dep))
                  (reduce into []))]
       ~(make-internal state collected false))))

(defn goal-maker-call
  [goal]
  `(~(goal-maker-symbol goal) ~@(->> goal
                                     goal-deps
                                     (map local-dep-symbol))))

(defn multi-maker-call
  [goal cases-states]
  (let [{:keys [selector cases] :as multi-meta} (multi-dep goal)]
    `(case ~selector
       ~@(mapcat
           #(vector % (make-internal (get cases-states %) % false))
           cases))))

(defn dependants
  [{:keys [rev-deps item-list]}]
  (letfn [(add-dependants [result dep]
            (->> dep
                 rev-deps
                 (map (partial add-dependants result))
                 (reduce into #{dep})))]
    (reduce add-dependants #{} item-list)))

(defmethod handle-goal :iterator-collector
  [goal in-state]
  (let [item-dep (item-dep goal)
        new-item-list [item-dep]
        up-state (-> in-state
                     (run-on-deps [(iteration-dep goal)])
                     (combine-maker-state
                       {:item-list new-item-list
                        :env #{item-dep}}))
        stored-keys [:item-list :walk-list]
        collected-state (run-on-deps
                          (-> up-state
                              (merge (select-keys stored-keys
                                                  (create-maker-state nil))))
                          [(collected-dep goal)])
        local-dependants (dependants (assoc collected-state
                                       :item-list new-item-list))
        collector-maker (collector-maker-call
                          goal
                          (update collected-state
                                  :walk-list #(filter local-dependants %)))
        deps (map iteration-dep new-item-list)
        result-state
        (-> collected-state
            (assoc :env (:env up-state))
            (merge (select-keys up-state                    ;;restore state
                                stored-keys))
            (update :walk-list concat
                    (remove local-dependants
                            (:walk-list collected-state)))
            (update :env into (set/difference (:env collected-state)
                                              local-dependants)))]
    (combine-maker-state
      result-state
      {:bindings {goal [(local-dep-symbol goal) collector-maker]}
       :walk-list [goal]
       :rev-deps (->> deps
                      (map #(vector % [goal]))
                      (into {}))
       :env #{goal}})))

(defmethod handle-goal :collector
  [goal {:keys [env] :as in-state}]
  (let [stored-keys [:item-list :walk-list]
        collected-state (run-on-deps
                          (-> in-state
                              (merge (select-keys stored-keys
                                                  (create-maker-state nil))))
                          [(collected-dep goal)])
        new-item-list (:item-list collected-state)
        local-dependants (dependants collected-state)
        collector-maker (collector-maker-call
                          goal
                          (update collected-state
                                  :walk-list #(filter local-dependants %)))
        deps (map iteration-dep new-item-list)
        up-state (-> collected-state
                     (assoc :env env)
                     (merge (select-keys in-state           ;;restore state
                                         stored-keys))
                     (run-on-deps deps)
                     (update :walk-list concat
                             (remove local-dependants
                                     (:walk-list collected-state)))
                     (update :env into (set/difference (:env collected-state)
                                                       local-dependants)))]
    (combine-maker-state
      up-state
      {:bindings {goal [(local-dep-symbol goal) collector-maker]}
       :walk-list [goal]
       :rev-deps (->> deps
                      (map #(vector % [goal]))
                      (into {}))
       :env #{goal}})))

(defmethod handle-goal :multi
  [goal {:keys [] :as in-state}]
  (let [{:keys [selector cases] :as multi-meta} (multi-dep goal)
        selector-state (run-on-deps in-state [selector])
        cases-states (->> cases
                          (map #(vector % (run-on-deps
                                            (assoc selector-state
                                              :walk-list [])
                                            [%])))
                          (into {}))

        cases-states-list (map #(get cases-states %) cases)
        combined-cases-state
        (reduce combine-maker-state selector-state cases-states-list)

        common-set (->> cases-states-list
                        (mapv (comp set :walk-list))
                        (reduce set/intersection))
        common-walk-list (->> combined-cases-state
                              :walk-list
                              (filter common-set))]
    (-> selector-state
        (combine-maker-state (-> combined-cases-state
                                 (assoc :walk-list common-walk-list)))
        (combine-maker-state
          {:bindings {goal [(local-dep-symbol goal)
                            (multi-maker-call goal
                                              (reduce-kv
                                                (fn [acc k v]
                                                  (assoc acc
                                                    k
                                                    (update v
                                                            :walk-list
                                                            (partial
                                                              remove
                                                              common-set))))
                                                {}
                                                cases-states))]}
           :rev-deps (->> (conj cases selector)
                          (map #(vector % [goal]))
                          (into {}))
           :walk-list [goal]
           :env #{goal}}))))

(defmethod handle-goal :default
  [goal {:keys [item-list] :as in-state}]
  (let [dependencies-state (run-on-deps in-state (goal-deps goal))]
    (-> dependencies-state
        (combine-maker-state
          {:bindings {goal [(local-dep-symbol goal) (goal-maker-call goal)]}
           :rev-deps (reverse-dependencies goal)
           :walk-list [goal]
           :env #{goal}}))))

(defn load-depencies
  "Call 'require' on every given namespace if necessary"
  [namespaces]
  #_(doseq [r namespaces]
      (try
        (ns-name r)                                         ;; TODO any better way to detect an unloaded ns?
        (catch Throwable _
          (require r)))))

(defn make-internal
  [{:keys [walk-list bindings] :as state} goal fail-on-opens]
  `(let [~@(->> walk-list
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
       pprint/pprint)
  form)

(defmacro make-with
  "Make a goal out of the environment"
  [goal env log-fn]
  (let [state (-> env keys set create-maker-state (assoc :log-fn log-fn))]
    (-> (run-on-deps state [goal])
        (make-internal goal true)
        (cond->
          log-fn print-generated-code))))

(defmacro make
  [goal]
  `(make-with ~goal ~&env nil))

(def #^{:macro true} *- #'make)

(defn log-fn
  [& args]
  (pprint/pprint (->> args
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
                (map (juxt (comp local-dep-symbol alias->fqn first)
                           second))
                (reduce into []))]
     ~@body))

(defmacro defgoal
  [the-name deps & body]
  `(do
     (defn ~the-name [~@(map dep-param-symbol deps)]
       ~@body)
     (alter-meta! (var ~the-name)
                  assoc
                  :goal true
                  :deps (quote ~(mapv (comp alias->fqn
                                            goal-maker-symbol)
                                      deps)))))

(defmacro declare-goal
  "Declare a goal"
  [the-name]
  `(defgoal ~the-name
            []
            (throw (ex-info ~(str "Goal definition is missing "
                                  *ns* "/" the-name)
                            {:type ::goal-runtime}))))
