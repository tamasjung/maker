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
  [goal-var]
  (-> goal-var meta ::promise-chan))

(defn- goal-realisation
  "For params"
  [{:keys [env context-ns in-go]} goal-var]
  (let [goal-local (m/goal-var-goal-local context-ns goal-var)]
    (if (or (env goal-local)
            (-> goal-var meta ::m/defgoalfn))
      goal-local
      (if in-go
        `(let [res# (clojure.core.async/<! ~goal-local)]
           (if (instance? Throwable res#)
             (throw res#)
             res#))
        `(let [res# (clojure.core.async/<!! ~goal-local)]
           (if (instance? Throwable res#)
             (throw res#)
             res#))))))

(defmulti render-assignment (fn [_ctx {:keys [goal-var] :as goal-model}]
                              (cond
                                (or (promise-chan-flag? goal-var)
                                    (-> goal-var meta ::m/defgoalfn))
                                :direct

                                :else :in-go)))

#_(defmethod goal-maker-call [::sequential ::async-goal-channel]
    [ctx end-goal goal-map]
    `(clojure.core.async/<!! ~(m/goal-maker-call ctx end-goal goal-map)))

(defmethod render-assignment :direct
  [ctx goal-model]
  (m/render-assignment ctx goal-model))


#_(defmethod goal-maker-call :in-thread
    [ctx end-goal goal-map]
    `(let [r# (clojure.core.async/promise-chan)]
       (clojure.core.async/thread
         (try
           (clojure.core.async/put! r#
                                    ~(m/goal-maker-call ctx end-goal goal-map))
           (catch Throwable th#
             (clojure.core.async/put! r# th#))))
       r#))

(defmethod render-assignment :in-go
  [ctx goal-model]
  (let [[local call] (m/render-assignment ctx goal-model)]
    [local
     `(let [r# (clojure.core.async/promise-chan)]
        (clojure.core.async/go
          (try
            (clojure.core.async/put! r#
                                     ~call)
            (catch Throwable th#
              (clojure.core.async/put! r# th#))))
        r#)]))

(defn- make>-with
  [goal env]
  (let [env (or env {})]
    `(clojure.core.async/go
       (try
         (a/<! ~(m/make-with {:render-assignment-fn render-assignment
                              :var-to-local-fn (partial m/goal-var-goal-local *ns*)
                              :goal-realisation-fn (partial goal-realisation)
                              :context-ns *ns*
                              :in-go true
                              :env env}
                             goal
                             env))
         (catch Throwable th#
           th#)))))

(defmacro make>
  [goal]
  (make>-with goal &env))


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

(defn take-in!!
  ([ch msec]
   (valid?? (a/alt!! ch ([v] v)
                     (a/timeout msec) ([] (ex-info "Timed out" {:ch (str ch)}))))))


