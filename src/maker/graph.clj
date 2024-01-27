(ns maker.graph
  (:require [clojure.set :as set]))

(defn first-topsorted
  "Next step of a topological sorting of a directed graph.
  Throws exception if the graph contains any cycles."
  ([successors-fn {:keys [stack stack-set sorted-set]
                   :as state}]
   (loop [sorted-set sorted-set
          stack stack
          stack-set stack-set]
     (if (empty? stack)
       [nil state]
       (let [v (peek stack)]
         (if-let [first-unsorted-successor (->> (successors-fn v)
                                                (remove sorted-set)
                                                first)]
           (if (stack-set first-unsorted-successor)
             (throw (ex-info "Circular dependency: " {:circular v}))
             (recur sorted-set
                    (conj stack first-unsorted-successor)
                    (conj stack-set first-unsorted-successor)))
           [v {:stack (pop stack)
               :stack-set (disj stack-set v)
               :sorted-set (conj sorted-set v)}]))))))

(defn visited-seq
  [children-fn n {:keys [perm-set temp-set] :as state}]

  (cond
    (perm-set n)
    [nil state]

    (temp-set n)
    (throw (ex-info "circle" n))

    :else
    (if-let [children (seq (children-fn n))]
      (let [next-state (update state :temp-set conj n)
            sorted-results (reductions (fn [[_ state] m]
                                         (visited-seq children-fn m state))
                                       (list () next-state)
                                       children)]
        (concat (lazy-seq (list (concat (->> sorted-results
                                             (map first)
                                             (apply concat))
                                        (list n))))
                (lazy-seq
                  (-> sorted-results last second
                      (update :temp-set disj n)
                      (update :perm-set conj n)
                      vector))))
      [[n] (update state :perm-set conj n)])))

(defn top-sorting
  [successor-fn state]
  (lazy-seq
    (let [[v new-state :as i] (first-topsorted successor-fn state)]
      (when (some? v)
        (cons i (top-sorting successor-fn new-state))))))

(defn collect-closest-independents
  "Collect recursively those nodes which are not dependent-pred and none of their children are."
  [children-fn dependent-pred node]
  (let [m (memoize (fn [f a-node]
                     (if (dependent-pred a-node)
                       [false #{}]
                       (let [children (children-fn a-node)
                             children-results (map (partial f f) children)
                             rec-independent (every? first children-results)]
                         [rec-independent
                          (if rec-independent
                            #{a-node}
                            (->> children-results
                                 (map second)
                                 (reduce set/union #{})))]))))]
    (m m node)))

