(ns maker.core
  (:require [maker.graph :as graph]
            [clojure.set :as set]
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
  (let [{:keys [ns name]} (:goal-meta goal)]
    (when name
      (->> [ns name]
           (string/join "/")
           symbol))))

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

(defn on-the-fly-goal-decl                                  ;TODO remove
  [ns whole-param-name]
  (let [s (gen-local-sym ns whole-param-name)]
    {:local s}))

(def on-the-fly? (complement :goal))                        ;TODO remove

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
        (on-the-fly-goal-decl ns whole-p)                   ;TODO remove, throw ex
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

(declare handle-goal)

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

(defn goal-maker-call
  [goal goal-deps]
  `(~(or (goal-maker-symbol goal)
         #_ "FIXME"
         #_(throw (ex-info "Missing goal definition" {:goal goal})))
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
           (if (symbol? s)
             (-> s
                 str
                 (string/replace #"^clojure.core/" "")
                 symbol)

             s)))
       pprint)
  form)

(def ^:dynamic *non-local-deps* nil)

(defn local-dependants
  [state env ns]
  (let [local-goals (->> env
                         keys
                         (map (partial goal-for-param ns))
                         (remove on-the-fly?))]
    (dependants-set state local-goals)))

(defn collect-non-local-deps!
  [env ns {:keys [rev-dep-goals] :as state} goal]
  (when *non-local-deps*
    (let [local-dependants (local-dependants state env ns)
          minimal-non-local-goal-set
          ((graph/border-fn goal-dep-goals
                            (complement local-dependants))
            goal)]
      (set! *non-local-deps*
            (into *non-local-deps* minimal-non-local-goal-set)))))

(defmacro make-with
  "Make a goal out of the environment"
  [goal-sym env log-fn]
  (let [state (-> env keys set create-maker-state (assoc :log-fn log-fn))
        goal (goal-for-param *ns* goal-sym)
        end-state (run-on-goals state [goal])]
    (collect-non-local-deps! env *ns* end-state goal)
    (-> end-state
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

#_
(defmacro defrelation
  [name & {:as relations}]
  (list `declare (with-meta name
                            (into {}
                                  (for [[k v] relations]
                                    [k `(quote ~v)])))))

(defn with-goal-meta
  [name]
  (with-meta (-> name
                 (str maker-postfix)
                 symbol)
             (meta name)))

(defmacro defgoal
  [name & fdecl]
  (apply list
         `defn
         (with-goal-meta name)
         fdecl))

(defmacro defgoal!
  [name & fdecl]
  (let [additional-params
        (binding [*non-local-deps* []]
          (eval (apply list 'defgoal name fdecl))
          (->> *non-local-deps*
               (map :local)
               vec))]
    (apply list 'defgoal name (into additional-params (first fdecl))
           (rest fdecl))))

(defmacro defgoal?
  [name]
  (list 'declare (with-goal-meta name)))

#_(defmacro with
    [pairs & body]
    (assert (-> pairs count even?))
    `(let [~@(->> pairs
                  (partition 2)
                  (map (juxt (comp local-dep-symbol)
                             second))
                  (reduce into []))]
       ~@body))