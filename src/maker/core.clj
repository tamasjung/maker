(ns maker.core
  (:require [clojure.set :as set]
            [clojure.string :as string]))

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

(defn some-contains
  [env-stack item]
  #_(assert (vector? env-stack) (str [env-stack item]))
  (some #(% item) env-stack))

(defn conj-top
  [env-stack item]
  (-> env-stack
      pop
      (conj (-> env-stack
                peek
                (conj item)))))

(defn map-longer 
  [f c1 c2]
  (lazy-seq
   (let [s1 (seq c1) s2 (seq c2)]
     (when (or s1 s2)
       (cons (f (first s1) (first s2))
             (map-longer f (rest s1) (rest s2)))))))

(defn merge-stack
  [s1 s2]
  (let []
    (vec (map-longer set/union s1 s2))))

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
                      whole))))

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
  (whole-symbol dep))

(defn goal-meta
  [goal]
  (-> goal
      goal-maker-symbol
      symbol->meta))

#_(def local-dep-symbol dep-param-symbol)

(defn local-dep-symbol
  "Returns the local bind symbol for a dependency"
  [dep]
  (-> dep whole-symbol alias->fqn split-fqn second symbol))

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

(defn relation
  [goal]
  (-> goal whole-symbol symbol->meta :relation))

(defn iteration-dep
  [goal]
  (-> goal whole-symbol symbol->meta :for))

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

(defn goal-maker-call
  "Creates the expression to make a goal by calling its function"
  [goal]
  (let [deps (-> goal goal-deps)
        locals (->> deps (map local-dep-symbol))]
    `(~(goal-maker-symbol goal) ~@(->> goal
                                       goal-deps
                                       (map local-dep-symbol)))))

(declare make-internal)

(defn relation-maker-call
  [goal {:keys [open-binding-list] :as state}]
  (let [rel (relation goal)]
    `(for [~@(->> open-binding-list
                  (map (juxt local-dep-symbol
                             (fn obm [a-goal]
                               (or (iteration-dep a-goal)
                                   (throw (IllegalStateException.
                                           (str "missing 'for' of "
                                                a-goal)))))))
                  (reduce into))]
       ~(make-internal state rel false))))

(defn create-maker-state
  [env]
  {:bindings []
   :env (or env [#{}])
   :reqs #{}
   :open-bindings #{}
   :open-binding-list []})

(defn combine-maker-state
  [new-state old-state]
  (reduce (fn _cmsr [acc [key comb-fn]]
            (update acc key comb-fn (key new-state)))
          old-state
          [[:bindings concat]
           [:reqs set/union]
           [:open-bindings set/union]
           [:open-binding-list concat]]))

(defn conj-top-dep-to-current-env
  [state goal]
  (-> state
      :env
      (conj-top (local-dep-symbol goal))
      (->> (assoc state :env))))

(defn handler-selector
  [goal state]
  (cond
    (iteration-dep goal) :iteration
    (relation goal) :relation
    :else :default))

(defmulti handle-goal handler-selector)

(defn run-on-deps
  [state deps]
  (if-let [dep (first deps)]
    (if (->> dep
             local-dep-symbol
             (some-contains (:env state)))
      (recur state (rest deps))
      (recur (handle-goal dep state)
             (rest deps)))
    state))

(defmethod handle-goal :iteration
  [goal old-state]
  (-> (combine-maker-state
       {:open-bindings #{goal}
        :open-binding-list [goal]
        :reqs #{(goal->namespace goal)}}
       old-state)
      (conj-top-dep-to-current-env goal)))

(defmethod handle-goal :relation
  [goal old-state]
  (let [stored-keys [:open-binding-list
                     :open-bindings
                     :bindings]
        relation-state (run-on-deps (-> old-state
                                        (merge (-> (create-maker-state nil) ;;overrides tmp
                                                   (select-keys stored-keys)))
                                        (update :env conj #{}))
                                    [(-> goal relation)])
        open-bindings-list (:open-binding-list relation-state)
        relation-maker (relation-maker-call goal relation-state)
        up-state (-> relation-state
                     (update :env pop)
                     (merge (select-keys old-state ;;restore state
                                         stored-keys))
                     (run-on-deps (map iteration-dep open-bindings-list)))]
    (-> (combine-maker-state
         {:bindings [[(local-dep-symbol goal) relation-maker]]
          :reqs #{(goal->namespace goal)}}
         up-state)
        (conj-top-dep-to-current-env goal))))

(defmethod handle-goal :default
  [goal old-state]
  (let [dependencies-state (run-on-deps old-state (goal-deps goal))]
    (-> (combine-maker-state;a normal/plain dependency
         {:bindings [[(local-dep-symbol goal) (goal-maker-call goal)]]
          :reqs #{(goal->namespace goal)}}
         dependencies-state)
        (conj-top-dep-to-current-env goal))))

(defn load-depencies
  "Call 'require' on every given namespace if necessary"
  [namespaces]
  #_(doseq [r namespaces]
    (try
      (ns-name r)                                           ;; TODO any better way to detect an unloaded ns?
      (catch Throwable _
        (require r)))))

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

(defn make-internal
  [state goal fail-on-opens]
  (let [{:keys [bindings requires open-bindings]} state]
    (when (and fail-on-opens
               (seq open-bindings))
      (throw (IllegalArgumentException.
              (str "Open binding remained: " (string/join ", " open-bindings)))))
    (load-depencies requires)
    `(let [~@(->> bindings
                  (apply concat))]
       ~(local-dep-symbol goal))))

(defmacro make-with
  "Make a goal out of the environment"
  [goal env]
  (-> goal
      (handle-goal (-> env keys set vector create-maker-state))
      (make-internal goal true)))

(defmacro make
  [goal]
  `(make-with ~goal ~&env))

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
