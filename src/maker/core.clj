(ns maker.core
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]))

(defn inj-munge
  "Injective munge" ;;...it will be.
  [s]
  (-> s
      ;(string/replace "+" "++")
      ;(string/replace "!" "+!")
      (string/replace "/" "!")
      ;(string/replace "_" "+_")
      (string/replace "." "_")))

;;env stack operations

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

(defn dep-declaration-check
  "Check if the dependency is in correct form"
  [dep]
  (assert (whole-symbol dep)
          (str dep " dependency declaration should be a symbol or "
               "has an :as keyword in it")))

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

#_(def local-dep-symbol dep-param-symbol)

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
       ((juxt :deps (comp first :arglists)))
       (some identity)))

(defn is-goal?
  "Check the meta for the :goal flag"
  [sym]
  (-> sym whole-symbol symbol->meta :goal))

(defn collector
  [goal]
  (-> goal whole-symbol goal-maker-symbol symbol->meta :collect))

(defn iteration-dep
  [goal]
  (-> goal whole-symbol goal-maker-symbol symbol->meta :for))

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
  {:bindings []
   :env (or env #{})
   :reqs #{}
   :items {}
   :item-list []})

(defn combine-items
  [m1 m2]
  (reduce-kv
   (fn [acc k v]
     (update acc k (fnil into []) v))
   m1
   m2))

(defn combine-maker-state
  [new-state old-state]
  (reduce (fn _cmsr [acc [key comb-fn]]
            (update acc key comb-fn (key new-state)))
          old-state
          [[:bindings concat]
           [:reqs set/union]
           [:items combine-items]
           [:item-list concat]]))

(defn conj-dep-to-current-env
  [state goal]
  (update state :env conj goal))

(defn handler-selector
  [goal state]
  (cond
    (iteration-dep goal) :iteration
    (collector goal) :collector
    :else :default))

(defmulti handle-goal handler-selector)

(defn run-on-deps
  [state deps]
  (if-let [dep (first deps)]
    (if (->> dep
             local-dep-symbol
             ((:env state)))
      (do (prn dep "====" state)
          (recur state (rest deps)))
      (do
        (prn dep "===>" state)
        (recur (handle-goal dep state)
               (rest deps))))
    state))

(defmethod handle-goal :iteration
  [goal old-state]
  (-> (combine-maker-state
       {:items {goal []}
        :item-list [goal]
        :reqs #{(goal->namespace goal)}}
       old-state)
      (conj-dep-to-current-env goal)))

(declare make-internal)

(defn collector-maker-call
  [goal {:keys [item-list] :as state}]
  (let [rel (collector goal)]
    `(for [~@(->> item-list
                  (map (juxt local-dep-symbol
                             (fn obm [a-goal]
                               (or (iteration-dep a-goal)
                                   (throw (IllegalStateException.
                                           (str "missing 'for' of "
                                                a-goal)))))))
                  (reduce into))]
       ~(make-internal state rel false))))

(defmethod handle-goal :collector
  [goal {:keys [env] :as old-state}]
  (let [stored-keys [:item-list
                     :items
                     :bindings]
        collector-state (run-on-deps (-> old-state
                                        (merge (-> (create-maker-state nil) ;;overrides tmp
                                                   (select-keys stored-keys))))
                                    [(-> goal collector)])
        item-list (:item-list collector-state)
        collector-maker (collector-maker-call goal collector-state)
        up-state (-> collector-state
                     (assoc :env env)
                     (merge (select-keys old-state ;;restore state
                                         stored-keys))
                     (run-on-deps (map iteration-dep item-list)))]
    (-> (combine-maker-state
         {:bindings [[(local-dep-symbol goal) collector-maker]]
          :reqs #{(goal->namespace goal)}}
         up-state)
        (conj-dep-to-current-env goal))))

(defn goal-maker-call
  "Creates the expression to make a goal by calling its function"
  [goal]
  (let [deps (-> goal goal-deps)
        locals (->> deps (map local-dep-symbol))]
    `(~(goal-maker-symbol goal) ~@(->> goal
                                       goal-deps
                                       (map local-dep-symbol)))))

(defmethod handle-goal :default
  [goal old-state]
  (let [dependencies-state (run-on-deps old-state (goal-deps goal))
        items (->> dependencies-state
                           :item-list
                           (map (comp (partial vector goal)
                                      vector))
                           (into {}))]
    (-> (combine-maker-state;a normal/plain dependency
         {:bindings [[(local-dep-symbol goal) (goal-maker-call goal)]]
          :reqs #{(goal->namespace goal)}
          :items items}
         dependencies-state)
        (conj-dep-to-current-env goal))))

(defn load-depencies
  "Call 'require' on every given namespace if necessary"
  [namespaces]
  #_(doseq [r namespaces]
    (try
      (ns-name r)                                           ;; TODO any better way to detect an unloaded ns?
      (catch Throwable _
        (require r)))))

(defn make-internal
  [state goal fail-on-opens]
  (let [{:keys [bindings requires items]} state]
    (when (and fail-on-opens
               (seq items))
      (throw (IllegalArgumentException.
              (str "Open binding remained: " (string/join ", " items)))))
    (load-depencies requires)
    `(let [~@(->> bindings
                  (apply concat))]
       ~(local-dep-symbol goal))))

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
  [goal env]
  (-> goal
      (handle-goal (-> env keys set create-maker-state))
      (make-internal goal true)))

(defmacro make
  [goal]
  `(make-with ~goal ~&env))

(defmacro prn-make-with ;; TODO can we do it somehow without duplication?
  [goal env]
  (-> goal
      (handle-goal (-> &env keys set create-maker-state))
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
