(ns maker.graph)
;http://stackoverflow.com/questions/27445876/is-there-a-simpler-way-to-memoize-a-recursive-let-fn

(defn dependents
  [children-fn]
  (let [deps (memoize (fn
                        [deps-fn node]
                        (let [children (children-fn node)]
                          (into children
                                (mapcat (partial deps-fn deps-fn) children)))))]
    (partial deps deps)))

(defn border-fn
  [children-fn pred]
  (let [m (memoize (fn
                     [f node]
                     (let [children (children-fn node)
                           found (filter pred children)
                           follow (remove pred children)]
                       (into found
                             (mapcat (partial f f) follow)))))]
    (partial m m)))
#_
(let [nodes {:a [:b :c]
             :b [:c :d :e]
             :c [:d]
             :d []
             :e [:f]
             :f nil}
      ms (border-fn nodes #{:c :d})]
  (->> :a
       ms
       distinct
       prn))
