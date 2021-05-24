(ns maker.async
  (:require [clojure.core.async :as a]
            [maker.core :as m]))

(defn to-promise-chan
  [v]
  (doto (a/promise-chan) (a/put! v)))

(defn pipe-to-promise-chan
  [ch]
  (let [res (a/promise-chan)]
    (a/pipe ch res)
    res))

(defn promise-chan-flag?
  [goal-map]
  (-> goal-map :goal-var meta ::promise-chan true?))

(defn- goal-realisation
  "For params"
  [env ctx {:keys [goal-local] :as goal-map}]
  (if (or (-> goal-local env)
          (-> goal-map :goal-var meta ::m/defgoalfn true?))
    goal-local
    (if (-> ctx ::in-go true?)
      `(let [res# (clojure.core.async/<! ~goal-local)]
         (if (instance? Throwable res#)
           (throw res#)
           res#))
      `(let [res# (clojure.core.async/<!! ~goal-local)]
         (if (instance? Throwable res#)
           (throw res#)
           res#)))))

(defmulti goal-maker-call (fn [_ctx _end-goal-map goal-map]
                            (cond
                              (or (promise-chan-flag? goal-map)
                                  (-> goal-map :goal-var meta ::m/defgoalfn true?))
                              :direct

                              :else :in-go)))

#_(defmethod goal-maker-call [::sequential ::async-goal-channel]
    [ctx end-goal goal-map]
    `(clojure.core.async/<!! ~(m/goal-maker-call ctx end-goal goal-map)))

(defmethod goal-maker-call :direct
  [ctx end-goal goal-map]
  (m/goal-maker-call ctx end-goal goal-map))


#_
(defmethod goal-maker-call :in-thread
  [ctx end-goal goal-map]
  `(let [r# (clojure.core.async/promise-chan)]
     (clojure.core.async/thread
       (try
         (clojure.core.async/put! r#
                                  ~(m/goal-maker-call ctx end-goal goal-map))
         (catch Throwable th#
           (clojure.core.async/put! r# th#))))
     r#))

(defmethod goal-maker-call :in-go
  [ctx end-goal goal-map]
  `(let [r# (clojure.core.async/promise-chan)]
     (clojure.core.async/go
       (try
         (clojure.core.async/put! r#
                                  ~(m/goal-maker-call ctx end-goal goal-map))
         (catch Throwable th#
           (clojure.core.async/put! r# th#))))
     r#))

(defmacro make>
  [goal]
  `(clojure.core.async/go
     (try
       (a/<! ~(m/make-with {:goal-maker-call-fn goal-maker-call
                            :goal-realisation-fn (partial goal-realisation (or &env {}))
                            :context-ns *ns*
                            ::in-go true}
                           goal
                           &env))
       (catch Throwable th#
         th#))))


(defmacro defgoal>
  [name & fdecl]
  (apply list 'maker.core/defgoal (vary-meta name assoc ::promise-chan true)
         fdecl))

(defmacro defgoal<
  [name & fdecl]
  (let [{:keys [doc params body]} (m/args-map [[:doc string?]
                                               [:params vector?]]
                                              fdecl)
        callback-body `[(let [result# (clojure.core.async/promise-chan)
                              ~'yield (fn yield-fn# [v#] (clojure.core.async/put! result# v#))]
                          ~@body
                          result#)]]
    `(defgoal> ~name
       ~@(m/rebuild-args doc params callback-body))))

(defn valid??
  [sth]
  (cond
    (nil? sth)
    (throw (ex-info "Nil is an invalid value of a channel." {}))

    (instance? Throwable sth)
    (throw sth)

    :default
    sth))

(defn take-in??
  ([ch msec]
   (valid?? (a/alt!! ch ([v] v)
                     (a/timeout msec) ([] (ex-info "Timed out" {:ch (str ch)}))))))


