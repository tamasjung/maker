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
