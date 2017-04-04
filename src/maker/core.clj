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

(def maker-postfix \*)

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
                   (throw (IllegalArgumentException.
                            (str "Missing dependency name, you may want to add :as to the parameter destructuring."))))
    (vector? dep) (let [[as whole] (take-last 2 dep)]
                    (if (and (= :as as)
                               (symbol? whole))
                      whole
                      (throw (IllegalArgumentException.
                               (str "Unrecognized vector param" dep ".")))))
    :default (throw (IllegalArgumentException.
                      (str "Unrecognized dependency:" dep ".")))))

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

(defn gen-local-sym
  [ns name]
  (if (= *ns* ns)
    (if (symbol? name)
      name
      (symbol name))
    (-> [ns name]
        (->> (string/join "/"))
        inj-munge
        symbol)))

(defn on-the-fly-goal-decl
  [ns whole-param-name]
  {:local (gen-local-sym ns whole-param-name)})

(defn goal-for-param
  [ns param]
  (when param
    (let [whole-p (whole-param param)
          refered-goal-name (-> whole-p
                                (str maker-postfix))
          refered-goal (-> refered-goal-name
                           symbol
                           (resolve-in ns))]
      (if-not refered-goal
        (on-the-fly-goal-decl ns whole-p)
        {:goal refered-goal
         :local (gen-local-sym
                  (-> refered-goal meta :ns)
                  (-> refered-goal meta :name str (without-end maker-postfix)))
         :goal-meta (meta refered-goal)}))))

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
  (related-goal goal #(or (:collect %)
                          (-> %
                              :name
                              str
                              (without-end \*)
                              (without-end \s)
                              symbol))))

(defn iteration-goal
  [goal]
  (related-goal goal #(when-let [for-val (:for %)]
                       (cond
                         (vector? for-val) (second for-val)
                         (symbol? for-val) for-val))))

(defn item-goal
  [goal]
  (related-goal goal #(when-let [for-val (:for %)]
                       (cond
                         (vector? for-val) (first for-val)
                         (symbol? for-val) (-> for-val
                                               str
                                               (without-end \s)
                                               symbol)))))

(def dynamic-selectors (atom {}))

(defn multi-goal-definition
  [goal]
  (let [{:keys [selector cases dynamic-selector] :as goal-meta}
        (-> goal :goal-meta)]
    (cond
      (and selector cases (not dynamic-selector))
      goal-meta

      dynamic-selector
      {:selector (:local goal)
       :cases (->> goal
                   :local
                   (get @dynamic-selectors)
                   :cases
                   (map first))
       :dynamic-selector true})))

(defn create-maker-state
  [env]
  {:bindings {}
   :local-env (or env #{})
   :no-circular-dep #{}
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
           [:no-circular-dep into #{}]
           [:walk-goal-list (comp distinct concat) []]
           [:log-fn #(or %1 %2) nil]]))

(defn handler-selector
  [goal _]
  (cond
    (-> goal :goal-meta :for) :iterator-collector
    (-> goal :goal-meta :params) :goal-fn
    (-> goal
        :goal-meta
        :dynamic-selector) :dynamic-selector
    (multi-goal-definition goal) :multi
    :else :default))

(defmulti handle-goal handler-selector)

(defn run-on-goals
  [state goals]
  (let [log-fn (or (:log-fn state)
                   (constantly nil))]
    (if-let [goal (first goals)]
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
  [goal state item-goal-list]
  (assert (= 1 (count item-goal-list)))
  (let [collected (collected-goal goal)]
    `(for [~(->> item-goal-list first local-dep-symbol)
           ~(-> goal iteration-goal local-dep-symbol)]
       ~(make-internal state collected))))

(defn goal-maker-call
  [goal goal-deps]
  `(~(goal-maker-symbol goal) ~@(map local-dep-symbol goal-deps)))

(defn multi-maker-call
  [goal cases-states case-goals]
  (let [{:keys [selector]} (multi-goal-definition goal)]
    `(case ~selector
       ~@(mapcat
           #(vector (local-dep-symbol %)
                    (make-internal (get cases-states %) %))
           case-goals))))

(defn dependants
  [{:keys [rev-dep-goals]} item-goal-list]
  (letfn [(add-dependants [result dep-goal]
            (->> dep-goal
                 (get rev-dep-goals)
                 (map (partial add-dependants result))
                 (reduce into #{dep-goal})))]
    (reduce add-dependants #{} item-goal-list)))

(defmethod handle-goal :iterator-collector
  [goal in-state]
  (let [item-goal (item-goal goal)
        new-item-list [item-goal]
        up-state (-> in-state
                     (run-on-goals [(iteration-goal goal)])
                     (combine-maker-state
                       {:local-env #{(:local item-goal)}}))
        collected-state (run-on-goals
                          (assoc up-state :walk-goal-list [])
                          [(collected-goal goal)])
        local-dependants (dependants collected-state new-item-list)
        non-local-dependants (remove local-dependants
                                     (:walk-goal-list collected-state))
        collector-maker (collector-maker-call
                          goal
                          (-> collected-state
                              (update :walk-goal-list
                                      #(filter local-dependants %)))
                          new-item-list)
        dep-goals (reduce into [] [[(iteration-goal goal)]
                                   non-local-dependants])
        result-state
        (-> collected-state
            (assoc :local-env (:local-env up-state))
            (assoc :walk-goal-list (concat (:walk-goal-list up-state)
                                           non-local-dependants))
            (update :local-env into (set/difference (:local-env collected-state)
                                                    (->> local-dependants
                                                         (map :local)
                                                         set))))]
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
  (let [{:keys [selector cases]} (multi-goal-definition goal)
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
                        (reduce set/intersection #{}))
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

(defmethod handle-goal :dynamic-selector
  [goal in-state]
  (let [{:keys [selector cases]} (multi-goal-definition goal)
        goal-ns (-> goal :goal-meta :ns)
        selector-goal (goal-for-param goal-ns selector)
        case-goals (mapv (partial goal-for-param goal-ns) cases)
        selector-state (run-on-goals (update in-state
                                             :no-circular-dep
                                             disj
                                             selector-goal)
                                     [(update selector-goal
                                                       :goal-meta
                                                       dissoc
                                                       :dynamic-selector)])
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
                        (reduce set/intersection #{}))
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

(defn make-internal
  [{:keys [walk-goal-list bindings] :as _state} goal]
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
        (make-internal goal)
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

(defmacro pp-make
  [goal & [user-log-fn]]
  `(make-with ~goal ~&env ~(or (and user-log-fn
                                    (eval user-log-fn))
                               log-fn)))

(def #^{:macro true} pp*- #'pp-make)

(defmacro defrelation
  [name & {:as relations}]
  (list `declare (with-meta name
                            (into {}
                                  (for [[k v] relations]
                                    [k `(quote ~v)])))))

(defmacro defgoal
  [name & fdecl]
  (apply list
         `defn
         (with-meta (-> name
                        (str maker-postfix)
                        symbol)

                    (meta name))
         fdecl))

(defmacro defsel
  [sel-name & args]
  `(defgoal ~(with-meta sel-name {:dynamic-selector true}) ~@args))

(defmacro defcase
  [selector & [dispatch-val selected-goal]]
  (let [selector-symbol
        (cond
          (symbol? selector)
          selector

          (keyword? selector)
          (-> selector name symbol)

          :else
          (throw (ex-info "defcase's first parameter should be a symbol or a keyword"
                          {:selector selector})))]
    (swap! dynamic-selectors
           update-in
           [selector-symbol :cases]
           (fnil conj {})
           [dispatch-val (or selected-goal
                             dispatch-val)])
    nil))

#_(defmacro with
    [pairs & body]
    (assert (-> pairs count even?))
    `(let [~@(->> pairs
                  (partition 2)
                  (map (juxt (comp local-dep-symbol)
                             second))
                  (reduce into []))]
       ~@body))