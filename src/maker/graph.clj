(ns maker.graph)

(defn first-topsorted
  "Next step of a topological sort of a directed graph.
  Throws exception if the graph contains any cycles."
  ([successors-fn {:keys [stack stack-set sorted-set]
                   :as state}]
   (loop [sorted-set sorted-set
          stack-vec stack
          stack-set stack-set]
     (if (empty? stack-vec)
       [nil state]
       (let [v (peek stack-vec)]
         (if-let [first-unsorted-successor (->> (successors-fn v)
                                                (remove sorted-set)
                                                first)]
           (if (stack-set first-unsorted-successor)
             (throw (ex-info "Circular dependency: " {:circular v}))
             (recur sorted-set
                    (conj stack-vec first-unsorted-successor)
                    (conj stack-set first-unsorted-successor)))
           [v {:stack (pop stack-vec)
               :stack-set (disj stack-set v)
               :sorted-set (conj sorted-set v)}]))))))

(defn top-sorting
  [successor-fn state]
  (lazy-seq
    (let [[v new-state :as i] (first-topsorted successor-fn state)]
      (when (some? v)
        (cons i (top-sorting successor-fn new-state))))))