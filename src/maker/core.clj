(ns maker.core
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(defn inj-munge
  "Injective munge"
  [s]
  (-> s
      (string/replace #"\+" "++")
      (string/replace #"!" "+!")
      (string/replace #"/" "!")
      (string/replace #"_" "+_")
      (string/replace #"\." "_")))

(defn dep-declaration-check
  "Check if the dependency is in correct form"
  [dep]
  (assert (-> dep count (= 2))
          (str dep " dependency declaration expected to be a pair")))

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

(defn dep-param
  "Returns the parameter form."
  [dep]
  (if (sequential? dep)
    (first dep)
    (-> dep alias->fqn split-fqn second symbol)))

(defn goal-maker-symbol
  "Calculates the goal maker fn's symbol"
  [dep]
  (if (sequential? dep)
    (do
      (dep-declaration-check dep)
      (-> dep second))
    dep))

(defn local-dep-symbol
  "Returns the local bind symbol for a dependency"
  [dep]
  (-> dep
      goal-maker-symbol
      str
      inj-munge
      symbol))

(defn goal-deps
  "Reads the deps from the goal's meta"
  [goal]
  (-> (goal-maker-symbol goal)
      symbol->meta
      :deps))

(defn is-goal?
  "Check the meta for the :goal flag"
  [sym]
  (-> sym symbol->meta :goal))

(defn goal->namespace
  "Returns the namespace of the goal symbol"
  [goal]
  (-> goal
      goal-maker-symbol
      alias->fqn
      split-fqn
      first
      symbol))

(defn goal-maker-call
  "Creates the expression to make a goal by calling its function"
  [goal]
  `(~(goal-maker-symbol goal)
     ~@(->> goal goal-deps (map local-dep-symbol))))

(defn prerequisites
  "Returns
  - the pairs of local bindings and goal maker calls
  - the extended environment set and
  - the extended requires set"
  [goal the-env-set the-requires]
  (let [[dep-pairs dep-env requires]
        (->> goal
             goal-deps
             (remove (comp the-env-set local-dep-symbol))
             (reduce (fn [[given-pairs given-env given-reqs] dep]
                       (let [[pairs env reqs]
                             (prerequisites dep given-env given-reqs)]
                         [(concat given-pairs pairs)
                          (set/union given-env env)
                          (set/union given-reqs reqs)]))
                     [[]
                      the-env-set
                      the-requires]))]
    [(conj dep-pairs [(local-dep-symbol goal) (goal-maker-call goal)])
     (conj dep-env (local-dep-symbol goal))
     (conj requires (goal->namespace goal))]))

(defn load-depencies
  "Call 'require' on every given namespace if necessary"
  [namespaces]
  (doseq [r namespaces]
    (try
      (ns-name r)                                           ;; TODO any better way to detect an unloaded ns?
      (catch Throwable _
        (require r)))))

(defmacro defgoal
  "Define a goal"
  [the-name deps & body]
  `(do
     (defn ~the-name [~@(map dep-param deps)]
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
            (throw (ex-info "Goal definition is missing" {:goal-name ~the-name
                                                          :ns ~*ns*}))))

(defmacro make
  "Make a goal out of the environment"
  [goal]
  #_(when (-> goal is-goal? not)
      (throw (ex-info "Not a goal" {:goal goal})))
  (let [[pairs _ requires]
        (prerequisites goal
                       (-> &env keys set)                   ;; TBD sure?
                       #{})]
    (load-depencies requires)

    `(let [~@(->> pairs
                  reverse
                  (apply concat))]
       ~(local-dep-symbol goal))))

(defmacro with
  "Create an environment for making goals by binding fully-qualified symbols
  virtually."
  [pairs & body]
  (assert (-> pairs count even?) "With expected even number of forms in the first argument")
  `(let [~@(->> pairs
                (partition 2)
                (map (juxt (comp local-dep-symbol alias->fqn first)
                           second))
                (reduce into []))]
     ~@body))
