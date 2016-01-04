(ns maker.ctx-call
  (:require [clojure.set :as set]))

(def choose-arity first)

(defmacro *-
  [fn-sy]
  (let [arglists# (eval `(-> ~fn-sy var meta :arglists))
        ctx-set# (-> &env keys set)
        args-set# (-> arglists# choose-arity set)]
    (if-not (set/subset? args-set# ctx-set#)
      (throw (ex-info "Missing some args to call" {:fn fn-sy
                                                   :missings (set/difference args-set# ctx-set#)}))
      `(~fn-sy ~@(choose-arity arglists#)))))

