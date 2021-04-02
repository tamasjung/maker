(ns maker.graph)
(require '[clojure.pprint :refer [pprint]])                 ;FIXME remove
#_(defn topological-sort
    "Breath first traverse with check if it is circular then a deduplication."
    ([successor-fn start]
     (let [result-with-dups
           (loop [[[n circular?] & rest-nodes] [[start #{}]]
                  result ()]
             (if-not n
               result
               (if (circular? n)
                 (throw (ex-info "Circular" {:vertex n}))
                 (let [successors (successor-fn n)
                       succ-circular? (conj circular? conj)]
                   (recur (->> successors
                               (map #(vector % succ-circular?))
                               (into rest-nodes))
                          (conj result n))))))]
       (pprint (count result-with-dups))
       (loop [[v & rest-vs] result-with-dups
              repeated? #{}
              result ()]
         (if v
           (if (repeated? v)
             (recur rest-vs repeated? result)
             (recur rest-vs (conj repeated? v) (conj result v)))
           result)))))

(defn topsort-component
  "Topological sort of a component of a (presumably) directed graph.
  Returns nil if the graph contains any cycles. See loom.alg/topsort
  for a complete topological sort"
  ([successors start]
   (loop [seen #{}
          explored #{}
          result []
          stack [start]]
     (if (empty? stack)
       result
       (let [v (peek stack)
             seen (conj seen v)
             us (remove explored (successors v))]
         (if (seq us)
           (if-let [circular (some seen us)]
             (throw (ex-info "Circular" {:circular circular}))
             (recur seen explored result (conj stack (first us))))
           (recur seen (conj explored v) (conj result v) (pop stack))))))))
#_
(set! *warn-on-reflection* true)
(defn tsj
  ([successor-fn start]
   (tsj successor-fn start (java.util.ArrayList.) (java.util.HashSet.)
        #{}))
  ([successor-fn start   ^java.util.ArrayList result ^java.util.HashSet seen
    path]
   ;(prn start)
   (if (path start)
     (throw (ex-info "Circ" {}))
     (when-not (.contains seen start)
       (doto seen (.add start))
       ;(doto path (.push start))
       ;(prn (count path))
       (let [path (conj path start)]
         (doseq [s (successor-fn start)]
           (tsj successor-fn s result seen path)))
       ;(doto path (.pop))
       (doto result (.add start))))))


(defn topsort-component-j
  "Topological sort of a component of a (presumably) directed graph.
  Returns nil if the graph contains any cycles. See loom.alg/topsort
  for a complete topological sort"
  ([successors start]
   (loop [seen (java.util.HashSet.)
          explored (java.util.HashSet.)
          result (java.util.ArrayList.)
          stack (java.util.ArrayDeque. ^java.util.List [start])]
     (if (.isEmpty stack)
       result
       (let [v (.peek stack)
             ;_ (prn v)
             _ (.add seen v)
             us (remove #(.contains explored %) (successors v))]
         (if (seq us)
           (if-let [circular (some #(.contains seen %) us)]
             (throw (ex-info "Circular" {:circular circular}))
             (recur seen explored result (doto stack (.push (first us)))))
           (recur seen (doto explored (.add v)) (doto result (.add v)) (doto stack .pop))))))))

(defn update-key!
  "Transform value in java.util.Map m under key k with fn f."
  ([^java.util.Map m k f]
   (.put m k (f (.get m k))))
  ([^java.util.Map m k f & args]
   (.put m k (apply f (.get m k) args))))

(defn topological-sort
  "Take an adjacency list representation of a graph (a map from node names to
   sequences of child node names), and return a topological ordering of the node
   names in linear time, or throw an error if the graph is cyclic.
   If include-leaves? is false the ordering will only include keys from child-map,
   and if true it will also include nodes only named as children in child-map."
  [child-map & [include-leaves?]]
  (let [e (java.util.HashMap. ^java.util.Map child-map)
        re (java.util.HashMap.)
        s (java.util.Stack.)]
    (doseq [[p children] child-map
            c children]
      (when include-leaves? (when-not (.containsKey e c) (.put e c nil)))
      (update-key! re c #(cons p %)))
    (while (not (.isEmpty e))
      ((fn dfs1 [n]
         (when (.containsKey e n)
           (let [nns (.get e n)]
             (.remove e n)
             (doseq [nn nns] (dfs1 nn)))
           (.push s n)))
       (first (keys e))))
    (let [candidate (reverse (seq s))]
      (doseq [c candidate
              r (.remove re c)]
        (when (.containsKey re r)
          (throw (IllegalArgumentException. (format "Graph contains a cycle containing %s and %s" c r)))))
      candidate)))

(require '[clojure.pprint :refer [pprint]])                 ;FIXME remove
#_(prn (topsort-component-j {0 [1 2]
                           1 [2]
                           2 []}
                          0))

#_
(let [n 1000
        g (->> (range n)
               (map #(vector % (range %)))
               (into {}))]
    ;(pprint g)
    (doseq [f [
               'topological-sort
               'topsort-component 'topsort-component-j
               'tsj
               ]]
      (print f ": ")
      (time (print (count ((resolve f) g (dec n)))))))
#_(pprint (topsort-component {1 [2 3]
                              2 [4]
                              3 [4]
                              4 []}
                             1))
#_
(let [n 1000
      g (->> (range n)
             (map #(vector % [(inc %)]))
             (into {n []}))]
  ;(pprint g)
  (doseq [f [
             'topological-sort
             'topsort-component 'topsort-component-j
             'tsj
             ]]
    (print f ": ")
    (time (print (count ((resolve f) g 0))))))
#_
(defn s [c]
  (let [a 1]
    (try (s (inc c))
         (catch StackOverflowError _ (+ a c)))))
