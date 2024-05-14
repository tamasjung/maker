(ns maker.core-test
  #?(:cljs (:require-macros [maker.core :refer [defgoal defgoal?]]
                            [maker.cljs-core :refer [defmulticase defcasegoal register-case
                                                     make defgoalfn with-config]]))
  (:require #?(:clj [clojure.test :refer :all])
            #?(:cljs [cljs.test :refer-macros [deftest is testing run-tests]])
            #?(:clj [maker.core :refer :all])
            #?(:clj [maker.dev :refer [trace ppr]])
            [clojure.test.check.generators :as gen]
            [ns2 :refer [ns2a' ns3a-proxy' ns2i' ns2b']]    ;the point is: ns3 shouldn't be required directly here ever
            [ns1 :refer [ns1a']]
            [clojure.string :as string]))
;-------------------------------------------------------------------------------

(defn simple'
  []
  "simple")

;; simple is called a 'goal'
;; simple' is the maker function
;; "simple" is the value of the goal
(deftest test-simple
  (is (= "simple"
         ;let's make the goal
         (make simple)
         ;the make macro expands to
         #_(let [simple (simple')]
             simple))))


;; to define a maker function just put a ''' at the end of name.
(defn other'
  [simple]
  (str simple "-other"))

;; a direct definition
(def other2' "other2")

;; the defgoal macro puts ''' at the end of the goal name
(defgoal another
  "Just another goal but now using the defgoal - the same effect."
  [other other2]
  (str other "-" other2 "-another"))


(deftest base-transitive-dep-test
  (is (= (make another)
         "simple-other-other2-another"
         (let [simple (simple')
               other (other' simple)
               other2 other2'
               another (another' other other2)]
           another)))

  ;; shadow the original definition simply with let
  (is (= (let [simple "changed"]
           (make another))
         "changed-other-other2-another")))


;-------------------------------------------------------------------------------

(deftest test-across-nss
  ;; for external namespaces maker munges the binding names,
  ;; check it with macroexpansion
  (is (= 222 (make ns2a)))
  ;with-goals is an extended let, works well with goals from another namespaces.
  (is (= 22 (let [ns1a 11]
              (make ns2a)))))

;-------------------------------------------------------------------------------

(def direct' "d")

(defn direct-ref' [^{:maker.core/goal direct} b]
  (string/upper-case b))

(deftest test-direct-goal
  (is (= (make direct-ref)
         "D")))

;-------------------------------------------------------------------------------
(declare dd')

(defn goal-with-dyn-dep' [dd] (+ 10 dd))

(deftest test-dynamic-goal
  (is (= (let [dd 1]
           (make goal-with-dyn-dep))
         11)))

;-------------------------------------------------------------------------------

(def call-counter (atom 0))

;; the next is a declaration of a goal without definition
(defgoal? iterator-item)

(defn factor'
  []
  (swap! call-counter inc)
  2)

(defn iterator-items'
  []
  (range 10))

(defn collected-item'
  [factor ns2a iterator-item]
  (* iterator-item factor))

;;Look at iterator-item, it was declared above and the actual value is defined
;;here locally.
(defgoal collected-items
  "A goal with a make call in it."
  [iterator-items]
  (map (fn [iterator-item]                                  ;binds to iterator-item goal, declared above
         (make collected-item))
       iterator-items))

(deftest test-static-collectors
  (reset! call-counter 0)
  (is (= (last (make collected-items))
         18))
  ;;factor' was called 10 times, usually this is not what you want...so read on.
  (is (= @call-counter 10)))

#?(:clj (defgoalfn collected-item-fn [iterator-item] collected-item))
;TBD What if the collected-item is a <>

#?(:clj (defn another-collected-items2'
          [iterator-items collected-item-fn]
          (map collected-item-fn iterator-items)))

#?(:clj (deftest iterator-with-goalfn
          (reset! call-counter 0)
          (is (= (last (make another-collected-items2))
                 18))
          (is (= 1 @call-counter))))

#?(:clj (defgoalfn g-fn [ns2i] ns2b))
;FIXME it doesn't work for cljs
#?(:clj (deftest cross-ns-goalfn
          (is (= "221" ((make g-fn) 1)))))

#?(:clj (defgoalfn inline-goal-fn [iterator-item] [collected-item]
                   (str collected-item)))
#?(:clj (deftest test-inline-defgoalfn
          (is (= ((make inline-goal-fn) 10) "20"))))



;;-------------------------------------------------------------------------------
;works with multimethods

(defn multi-dep'
  []
  "123")

(defmulti multi' {:arglists '([multi-dep])} count)

(defmethod multi' 3
  [multi-dep]
  (str "yes:" multi-dep))

(deftest test-multi
  (is (= (make multi)
         "yes:123")))

;-------------------------------------------------------------------------------

(def choice-env')

(defn choice-dep-a'
  []
  "a")

(defn choice-dep-b'
  [choice-dep-a]
  (str choice-dep-a "b"))

(defn choice-add'
  []
  "add")


(defn choice-dispatch'
  [choice-env choice-add]
  choice-env)

(defmulticase choice choice-dispatch)

(defn choice1'
  [choice-dep-a choice-add]
  (str choice-dep-a choice-add "1"))

(register-case choice :choice1 choice1)

;or defn and register-case in one single step
(defcasegoal choice :choice2
             [choice-dep-b]
             (str choice-dep-b "2"))

(defn end'
  [choice]
  (str choice ":end"))

; b/c choice has the goal type m/case meta, the expansion of creating it will be
; (case ..) and the return value of 'choice' is used as the dispatcher and the
; matching choice (in our case choice1) will be 'made'.
; Check the expansion of make below.
; if you were wandering why are those inline fn definitions they are b/c of
; avoiding too big fn body error.
(deftest choice-test
  (is (= (let [choice-env :choice1]
           (make end))
         "aadd1:end"))
  (is (= (let [choice-env :choice1]
           (make choice))
         "aadd1"))
  (is (= (let [choice-env :choice2]
           (make end))
         "ab2:end")))


;-------------------------------------------------------------------------------

(defn m' [] {:a 1 :b 2})
(defn v' [] [11 22])

;; maker works together with destructuring
(defn destr-goal'
  [{:keys [a b] :as m} [c :as v]]
  [a b m c v])

(deftest test-destr
  (is (= (make destr-goal)
         [1 2 {:a 1 :b 2} 11 [11 22]])))

;; destructuring wiht dynamic goals

(defn d-destr-goal'
  [{:keys [a b] :as dm} [c :as dv]]
  [a b dm c dv])

(defgoal? dm)
(defgoal? dv)

(deftest test-d-destr
  (is (= (let [dm {:a 111 :b 222}
               dv [1 2]]
           (make d-destr-goal))
         [111 222 {:a 111 :b 222} 1 [1 2]])))



;-------------------------------------------------------------------------------

;; circular dependency is an error at compile time
#?(:clj
   (deftest circular-dep
     (is (re-find #"Circular"
                  (try
                    (eval '(do
                             (use 'maker.core)
                             (defn self'
                               [self])
                             (make self)))
                    (catch Throwable th
                      (str th)))))))

;-------------------------------------------------------------------------------
;Example support for reloaded framework.

(def stop-fns (atom ()))

(def stop-fn
  (partial swap! stop-fns conj))

(defn stop-system
  []
  (doseq [f @stop-fns]
    (f)
    (swap! stop-fns rest)))

;The three things above consist the generic support for a reloaded workflow.
;Find below the sample 'components'.

(defgoal config
  []
  (stop-fn #(println "stop the config."))
  "the config")

(defgoal db-conn
  [config]
  (stop-fn #(println "stop the db-conn."))
  "the db-conn")

(deftest my-little-component-framework
  (is (= (make db-conn)
         "the db-conn"))
  (is (= (count @stop-fns)
         2))
  (is (= (with-out-str
           (stop-system))
         "stop the db-conn.\nstop the config.\n"))
  (is (= (count @stop-fns)
         0)))

;-------------------------------------------------------------------------------
#?(:clj
   (deftest missing-def
     (is (re-find #"Undefined dependency"
                  (try
                    (eval '(do (use 'maker.core)
                               (defgoal a [b])
                               (make a)))
                    (catch Throwable ei
                      (-> ei (.getCause) str)))))

     (is (= 'aaaa'
            (try
              (eval '(do (use 'maker.core)
                         (defgoal? aaaa)
                         (make aaaa)))
              (catch Throwable ei
                (-> ei (.getCause) ex-data :meta :name)))))
     (is (re-find #"Undefined"
                  (try
                    (eval '(do (use 'maker.core)
                               (make non-existent)))
                    (catch Throwable th
                      (-> th (.getCause) str)))))))

;-------------------------------------------------------------------------------
;configuration support

(defgoal? config-a)
(defgoal configured
  [config-a]
  (str config-a "ured!!!"))

(defgoal? profile)
(defgoal my-config
  [profile]
  {:maker.core-test/config-a (str (name profile) " is config")})

#?(:clj
   (deftest test-config
     (let [profile :staging]
       (with-config [(let [profile :test]
                       (keys (make my-config)))
                     (make my-config)]
                    (is (= (make configured)
                           "staging is configured!!!"))))))

(def direct-config {:maker.core-test/config-a "config"})

#?(:clj
   (deftest test-direct-config
     ;if the compile time and runtime configs are different
     (with-config [[:maker.core-test/config-a] direct-config]
                  (is (= (make configured)
                         "configured!!!")))
     ;if the two config is the same
     (with-config direct-config
                  (is (= (make configured)
                         "configured!!!")))))

(defgoal shouldnt-be-called
  []
  (throw (ex-info "Never" {})))

(defgoal misconfigured
  [shouldnt-be-called config-a]
  (throw (ex-info "Never" {})))

#?(:clj
   (deftest check-fails-fast-for-wrong-config
     ;here the runtime config doesn't contain the 'promised' key
     ;the other goal constructors are not called although the order of parameters
     ;would implies that and it fails fast as it should
     (is (thrown-with-msg? Throwable #"Missing config keys"
                           (with-config [[:maker.core-test/config-a]
                                         {}]
                                        (make misconfigured))))))
#?(:clj
   (deftest with-non-required-ns
     (is (= (with-config {:ns3/ns3a 11}
                         (make ns3a-proxy))
            11))))

;-------------------------------------------------------------------------------
;redefinition support

(defn redef-a'
  []
  "a")

(defn redef-b'
  [redef-a]
  (str redef-a "b"))

(defn redef-c'
  []
  "c")

(defn redef-d'
  [redef-c]
  (str redef-c "d"))

(def opts {:redefs '{redef-c redef-b}})

(deftest test-redef

  (is (= "cd" (make redef-d)))

  (is (= "abd"
         #?(:cljs (make redef-d {:redefs {redef-c redef-b}}))
         #?(:clj (make redef-d {:redefs '{redef-c redef-b}}))
         #?(:clj (make redef-d opts)))))

;-------------------------------------------------------------------------------
;destruction

(def destr-m' {:aaa/destr-a "a"
               :destr-b [11 12]
               :destr-e {:destr-f "F"}})

#?(:clj (destruct-goals {:aaa/keys [destr-a]
                         {:keys [destr-f]} :destr-e
                         [destr-c destr-d :as destr-cd] :destr-b
                         :as destr-m2} destr-m))
#?(:clj
   (deftest test-destruct-goal
     (is (= "a" (make destr-a)))
     (is (= 11 (make destr-c)))
     (is (= [11 12] (make destr-cd)))
     (is (= (make destr-m) (make destr-m2)))
     (is (= "F" (make destr-f)))))


;-------------------------------------------------------------------------------


#?(:clj
   (deftest munge-test
     (are [s res] (and (-> s non-q-sym (= res))
                       (-> s non-q-sym non-q-sym-inv (= s)))
                  "aa" "aa"
                  "a.b/c" "a+_b+!c"
                  "ab/c" "ab+!c"
                  "+++/++" "+++++++!++++")))
#?(:clj
   (deftest generated-munge-test
     (let [strs (-> (gen/elements [\/ \. \_ \+ \! \a])
                    gen/vector
                    (->> (gen/fmap (partial apply str)))
                    (gen/sample 100))]
       (doseq [s strs]
         (is (= s (->> s non-q-sym non-q-sym-inv str)))))))
