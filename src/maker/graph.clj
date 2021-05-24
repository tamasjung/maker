(ns maker.graph)

(defn topsort-component
  "Topological sort of a directed graph.
  Throws exception if the graph contains any cycles.
  This is a slightly modified version of loom.alg-generic/topsort-component."
  ([successors stop? start]
   (loop [seen #{}
          explored #{}
          result []
          stack [start]]
     (if (empty? stack)
       result
       (let [v (peek stack)]
         (let [seen (conj seen v)
               us (remove explored (successors v))]
           (if (seq us)
             (if-let [circular (some seen us)]
               (throw (ex-info "Circular" {:circular circular}))
               (recur seen explored result (conj stack (first us))))
             (if (stop? v)
               (conj result v)
               (recur seen (conj explored v) (conj result v) (pop stack))))))))))