(ns maker.core
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]))

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

(defn inj-munge
  "Injective munge" ;;...it will be.
  [s]
  (-> s
      ;(string/replace "+" "++")
      ;(string/replace "!" "+!")
      (string/replace "/" "!")
      ;(string/replace "_" "+_")
      (string/replace "." "_")))

(defn map-longer
  [f c1 c2]
  (lazy-seq
   (let [s1 (seq c1) s2 (seq c2)]
     (when (or s1 s2)
       (cons (f (first s1) (first s2))
             (map-longer f (rest s1) (rest s2)))))))

(defn inj-munge-inv
  [s]
  (-> s
      (string/replace #"(?<!\+)_" ".")
      (string/replace #"(?<!\+)!" "/" )))

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
      goal-maker-symbol
      symbol->meta))

(defn local-dep-symbol
  "Returns the local bind symbol for a dependency"
  [dep]
  (-> dep whole-symbol goal-maker-symbol
      alias->fqn split-fqn second butlast ;; cut the *
      (->> (apply str)) symbol))

(defn goal-deps
  "Reads the deps from the goal's meta"
  [goal]
  (->> goal
       goal-meta
       ((juxt :deps (comp first :arglists))) ;; TBD the old :deps remains?
       (some identity)))

(defn collected-dep
  [goal]
  (-> goal whole-symbol goal-maker-symbol symbol->meta :collect))

(defn iteration-dep
  [goal]
  (-> goal whole-symbol goal-maker-symbol symbol->meta :for))

(defn multi-dep
  [goal]
  (let [{:keys [selector cases] :as goal-meta}
        (-> goal whole-symbol goal-maker-symbol symbol->meta)]
    (when (and selector cases)
      goal-meta)))

(defn goal->namespace
  "Returns the namespace of the goal symbol"
  [goal]
  (-> goal
      goal-maker-symbol
      alias->fqn
      split-fqn
      first
      symbol))

(defn goal->name
  "Returns the name (without ns) of the goal as symbol"
  [goal]
  (-> goal
      goal-maker-symbol
      alias->fqn
      split-fqn
      second
      symbol))

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
  (reduce (fn _cmsr [acc [key comb-fn default]]
            (update acc key (fnil comb-fn default) (key new-state)))
          old-state
          [[:bindings merge {}]
           [:rev-deps combine-items {}]
           [:env into #{}]
           [:no-circular-dep into #{}]
           [:walk-list (comp distinct concat) []]
           [:item-list (comp distinct concat) []]]))

(defn reverse-dependencies
  [dep]
  (->> dep
       goal-deps
       (map #(vector % [dep]))
       (into {})))

(defn handler-selector
  [goal state]
  (cond
    (iteration-dep goal) :iteration
    (collected-dep goal) :collector
    (multi-dep goal) :multi
    :else :default))

(defmulti handle-goal handler-selector)

(defn run-on-deps
  [state deps]
  (if-let [dep (some-> deps first local-dep-symbol)];;TODO check throughtout the ns for destructuring forms remain
    (if (-> state :env dep)
      (do (prn dep "===" state)
          (recur state (rest deps)))
      (if (-> state :no-circular-dep (get dep))
        (throw (IllegalStateException.
                (str "Circural dependency:"
                     dep
                     ", walk-path:"
                     (:walk-list state))))
        (do
          (prn dep "==>" state)
          (update (run-on-deps (handle-goal
                                dep
                                (update state :no-circular-dep conj dep))
                               (rest deps))
                  :no-circular-dep disj dep))))
    (do (prn "==|" state)
        state)))

(defmethod handle-goal :iteration
  [goal old-state]
  (-> (combine-maker-state
       {:item-list [goal]
        :walk-list [goal]
        :env #{goal}}
       old-state)))

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
  [goal state cases-states]
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

(defmethod handle-goal :collector
  [goal {:keys [env] :as in-state}]
  (let [stored-keys [:item-list :walk-list]
        collected-state (run-on-deps
                         (-> in-state
                             (merge (select-keys stored-keys
                                                 (create-maker-state nil))))
                         [(-> goal collected-dep)])
        new-item-list (:item-list collected-state)
        local-dependants (dependants collected-state)
        collector-maker (collector-maker-call
                         goal
                         (update collected-state
                                 :walk-list #(filter local-dependants %)))
        up-state (-> collected-state
                     (assoc :env env)
                     (merge (select-keys in-state ;;restore state
                                         stored-keys))
                     (run-on-deps (map iteration-dep new-item-list))
                     (update :walk-list concat
                             (remove local-dependants (:walk-list collected-state)))
                     (update :env into (set/difference (:env collected-state)
                                                       local-dependants)))]
    (combine-maker-state
     up-state
     {:bindings {goal [(local-dep-symbol goal) collector-maker]}
      :walk-list [goal]
      :env #{goal}})))

(defmethod handle-goal :multi
  [goal {:keys [] :as in-state}]
  (let [{:keys [selector cases] :as multi-meta} (multi-dep goal)
        selector-state (run-on-deps in-state [selector])
        cases-states (->> cases
                          (map #(vector % (run-on-deps selector-state [%])))
                          (into {}))
        item-list (->> cases-states
                       vals
                       (mapcat :item-list)
                       distinct)]

    (-> selector-state
        (combine-maker-state
         {:bindings {goal [(local-dep-symbol goal)
                           (multi-maker-call goal
                                             (assoc selector-state
                                                    :bindings {})
                                             cases-states)]}
          :rev-deps (->> item-list
                         (map #(vector % [goal]))
                         (into {}))
          :item-list item-list
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
      (ns-name r)                                           ;; TODO any better way to detect an unloaded ns?
      (catch Throwable _
        (require r)))))

(defn make-internal
  [{:keys [walk-list bindings] :as state} goal fail-on-opens]
  `(let [~@(->> walk-list
                reverse
                (map bindings)
                (reduce into []))]
     ~(local-dep-symbol goal)))

(defmacro make-with
  "Make a goal out of the environment"
  [goal env]
  (-> (run-on-deps (-> &env keys set create-maker-state) [goal])
      (make-internal goal true)))

(defmacro make
  [goal]
  `(make-with ~goal ~&env))

(defmacro prn-make-with ;; TODO can we do it somehow without duplication?
  [goal env]
  (-> (run-on-deps (-> &env keys set create-maker-state) [goal])
      (make-internal goal true)
      print-generated-code))

(defmacro prn-make
  [goal]
  `(prn-make-with ~goal ~&env))

(defmacro with
  "Create an environment for making goals by binding fully-qualified symbols
  virtually."
  [pairs & body]
  (assert (-> pairs count even?)
          "With expected even number of forms in the first argument")
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
