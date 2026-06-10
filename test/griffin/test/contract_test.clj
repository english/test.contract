(ns griffin.test.contract-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [griffin.test.contract :as c]
            [griffin.test.contract.protocol :as p]))

(defprotocol RemoteAPI
  :extend-via-metadata true
  (create-file [this file])
  (delete-file [this file])
  (file-exists? [this file]))

(def model
  (c/model
   {:protocols #{RemoteAPI}
    :methods [(c/method #'create-file
                        (fn [state [file]]
                          (if (not (get-in state [:files file]))
                            (c/return #{:ok}
                                      :next-state (update state :files conj file))
                            (c/return #{:error/file-exists}
                                      :next-state state)))
                        :args (fn [_state] (gen/tuple gen/string)))
              (c/method #'delete-file
                        (fn [state [file]]
                          (c/return #{:ok}
                                    :next-state (update state :files disj file)))
                        :requires (fn [state] (seq (:files state)))
                        :precondition (fn [state [file]] (contains? (:files state) file))
                        :args (fn [state] (gen/tuple (gen/elements (:files state)))))
              (c/method #'file-exists?
                        (fn [state [file]]
                          (let [exists? (boolean (get-in state [:files file]))]
                            (c/return (s/with-gen (fn [x] (= exists? x))
                                        (fn [] (gen/return exists?)))
                                      :next-state state)))
                        :args (fn [_state] (gen/tuple gen/string)))]

    :initial-state (fn []
                     {:files #{}})}))

(defn good-impl []
  (let [state (ref {:files #{}})]
    (reify
      RemoteAPI
      (create-file [_this f]
        (dosync
          (if (not (get (:files @state) f))
            (do
              (commute state update :files conj f)
              :ok)
            :error/file-exists)))
      (delete-file [_this f]
        (dosync
          (commute state update :files disj f)
          :ok))
      (file-exists? [_this f]
        (boolean (get (:files @state) f))))))

(defn bad-impl []
  (reify
    RemoteAPI
    (create-file [_this _f]
      :ok)
    (delete-file [_this _f]
      (throw (UnsupportedOperationException. "method not implemented")))
    (file-exists? [_this _f]
      false)))

(deftest model-works
  (is (:pass? (tc/quick-check 100 (c/test-model model)))))

(deftest mocks-work
  (let [mock (c/mock model)]
    (is (= :ok (create-file mock "hello")))
    (is (= :error/file-exists (create-file mock "hello")))))

(defn state-impl-is-thread-safe [state]
  (let [mock (c/mock model :mock-state state)]
    (is (= {true 100}
           (frequencies
            (map deref
                 (doall (map (fn [fname]
                               (future
                                 (create-file mock fname)
                                 (file-exists? mock fname)))
                             (range 100)))))))))

(deftest mock-state-is-thread-safe
  (doseq [state [(c/ephemeral-state) (c/ref-state (ref {}))]]
    (testing state
      (state-impl-is-thread-safe state))))

(deftest verify-works
  (let [ret (tc/quick-check 100 (c/verify model good-impl))]
    (is (:pass? ret) ret)))

(deftest verify-catches-errors
  (let [ret (tc/quick-check 100 (c/verify model bad-impl))]
    ;; (println "ret:" ret)
    (is (find ret :pass?) ret)
    (is (false? (:pass? ret)) ret)))

(deftest verify-num-calls-opt-works
  (let [num-calls 123
        orig-gen-calls c/gen-calls]
    (with-redefs [;; remove non-determinism in gen/large-integer* so that gen-calls always returns the max
                  gen/large-integer* (fn [{:keys [min max]}]
                                       (gen/return max))
                  c/gen-calls (fn [& args]
                                (gen/fmap (fn [calls]
                                            (is (= num-calls (count calls)))
                                            calls)
                                          (apply orig-gen-calls args)))]
      (is (:pass? (tc/quick-check 1 (c/verify model good-impl :num-calls num-calls)))))))

(deftest test-proxy
  (let [good-mock (c/test-proxy model (good-impl))]
    (is (= :ok (create-file good-mock "/foo")))
    (is (= :error/file-exists (create-file good-mock "/foo"))))

  (let [bad-mock (c/test-proxy model (bad-impl))]
    (is (= :ok (create-file bad-mock "/foo")))
    (is (thrown? Exception (create-file bad-mock "/foo")))))

(deftest shrinking-produces-valid-calls
  (->> (rose/seq (gen/call-gen (c/gen-calls model (p/initial-state model)) (random/make-random) 100))
       (take 1000)
       (run! (fn [calls]
               (reduce (fn [state {:keys [method args return]}]
                         (is (p/requires method state) "shrunk state obeys requires")
                         (is (p/precondition method state args) "shrunk state obeys preconditions")
                         (let [state' (p/next-state (p/return method state args))]
                           (is (= state' (p/next-state return)) "shrunk state is correct")
                           state'))
                       (p/initial-state model)
                       calls)))))

(deftest shrinking-finds-smallest-case
  (let [ret (tc/quick-check 100 (c/verify model bad-impl))
        smallest (first (:smallest (:shrunk ret)))]
    (is (:fail ret) ret)
    (is (:shrunk ret) ret)
    (is smallest (:shrunk ret))
    ;; the smallest possible failing cases for bad-impl all involve 2 calls:
    ;; - create the same file twice
    ;; - create a file then delete it (note the model doesn't let us delete until we've created a file)
    ;; - create a file then ask if it exists
    (is (= 2 (count smallest)) smallest)
    (is (not= smallest (:fail ret)) ret)))

(let [broken-model (c/model
                     {:protocols #{RemoteAPI}
                      :methods [(c/method #'create-file
                                          (fn [state [file]]
                                            (if (not (get-in state [:files file]))
                                              (c/return #{:ok}
                                                        :next-state (update state :files conj file))
                                              (c/return #{:error/file-exists}
                                                        :next-state state)))
                                          :args (fn [_state] (gen/tuple gen/string)))
                                (c/method #'delete-file
                                          (fn [state [file]]
                                            (c/return #{:ok}
                                                      :next-state (update state :files disj file)))
                                          :requires (fn [state] (seq (:files state)))
                                          :precondition (fn [state [file]] (contains? (:files state) file))
                                          :args (fn [state] (gen/tuple (gen/elements (:files state)))))
                                (c/method #'file-exists?
                                          (fn [state [file]]
                                            (let [exists? (boolean (get-in state [:files file]))]
                                              (c/return (s/with-gen (fn [x] (= exists? x))
                                                                    (fn [] (gen/return exists?)))
                                                        :next-state state)))
                                          :args (fn [_state]
                                                  ;; Not a gen
                                                  (s/tuple string?)))]

                      :initial-state (fn []
                                       {:files #{}})})]

  (deftest broken-model-test
    ;; enough runs that some generated call sequence is near-certain to
    ;; include the broken file-exists? method
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":args must return a generator for*"
         (tc/quick-check 100 (c/test-model broken-model))))))

;;; thrown outcomes / fault injection

(s/def ::error #{:error/timeout :error/conn-reset})

(defprotocol FlakyStore
  :extend-via-metadata true
  (put-item [this k])
  (has-item? [this k])
  (break-network [this])
  (fix-network [this]))

(def fault-model
  (c/model
   {:protocols #{FlakyStore}
    :methods [(c/method #'put-item
                        (fn [state [k]]
                          (if (:network-down? state)
                            (c/throws (c/ex-spec (s/keys :req [::error]))
                                      :next-state state)
                            (c/return #{:ok}
                                      :next-state (update state :items conj k))))
                        :args (fn [_state] (gen/tuple gen/string)))
              (c/method #'has-item?
                        (fn [state [k]]
                          (if (:network-down? state)
                            (c/throws (c/ex-spec (s/keys :req [::error]))
                                      :next-state state)
                            (let [exists? (contains? (:items state) k)]
                              (c/return (s/with-gen (fn [x] (= exists? x))
                                          (fn [] (gen/return exists?)))
                                        :next-state state))))
                        :args (fn [_state] (gen/tuple gen/string)))
              (c/method #'break-network
                        (fn [state _args]
                          (c/return #{:ok}
                                    :next-state (assoc state :network-down? true)))
                        :args (fn [_state] (gen/return [])))
              (c/method #'fix-network
                        (fn [state _args]
                          (c/return #{:ok}
                                    :next-state (assoc state :network-down? false)))
                        :args (fn [_state] (gen/return [])))]
    :initial-state (fn []
                     {:items #{}
                      :network-down? false})}))

(defn good-flaky-impl []
  (let [state (atom {:items #{} :network-down? false})]
    (reify
      FlakyStore
      (put-item [_this k]
        (if (:network-down? @state)
          (throw (ex-info "timed out" {::error :error/timeout}))
          (do (swap! state update :items conj k)
              :ok)))
      (has-item? [_this k]
        (if (:network-down? @state)
          (throw (ex-info "connection reset" {::error :error/conn-reset}))
          (contains? (:items @state) k)))
      (break-network [_this]
        (swap! state assoc :network-down? true)
        :ok)
      (fix-network [_this]
        (swap! state assoc :network-down? false)
        :ok))))

(defn undeclared-throw-impl
  "Throws even when the model says the network is up"
  []
  (reify
    FlakyStore
    (put-item [_this _k]
      (throw (ex-info "corrupted read" {:cause :disk})))
    (has-item? [_this _k] false)
    (break-network [_this] :ok)
    (fix-network [_this] :ok)))

(defn never-throws-impl
  "Returns values where the model expects a throw"
  []
  (let [state (atom {:items #{}})]
    (reify
      FlakyStore
      (put-item [_this k]
        (swap! state update :items conj k)
        :ok)
      (has-item? [_this k]
        (contains? (:items @state) k))
      (break-network [_this] :ok)
      (fix-network [_this] :ok))))

(deftest ex-spec-works
  (let [spec (c/ex-spec (s/keys :req [::error]))]
    (is (s/valid? spec (ex-info "boom" {::error :error/timeout})))
    (is (not (s/valid? spec (ex-info "boom" {:wrong :data}))))
    (is (not (s/valid? spec :not-an-exception)))
    (is (every? #(s/valid? spec %) (gen/sample (s/gen spec))))))

(deftest fault-model-works
  (is (:pass? (tc/quick-check 100 (c/test-model fault-model)))))

(deftest mocks-throw-declared-errors
  (let [mock (c/mock fault-model)]
    (is (= :ok (put-item mock "hello")))
    (is (true? (has-item? mock "hello")))
    (is (= :ok (break-network mock)))
    (let [e (try (put-item mock "world") (catch Exception e e))]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (s/valid? ::error (::error (ex-data e)))))
    (is (= :ok (fix-network mock)))
    ;; the failed put did not mutate state
    (is (false? (has-item? mock "world")))))

(deftest mock-faults-are-deterministic-with-seed
  (let [errors (for [_ (range 2)]
                 (let [mock (c/mock fault-model :seed 42)]
                   (break-network mock)
                   (ex-data (try (put-item mock "x") (catch Exception e e)))))]
    (is (apply = errors))
    (is (every? ::error errors))))

(deftest verify-accepts-conforming-throws
  (let [ret (tc/quick-check 100 (c/verify fault-model good-flaky-impl))]
    (is (:pass? ret) ret)))

(deftest verify-rejects-undeclared-throws
  (let [ret (tc/quick-check 100 (c/verify fault-model undeclared-throw-impl))]
    (is (false? (:pass? ret)) ret)
    ;; smallest failure is a single put-item call
    (is (= 1 (count (first (:smallest (:shrunk ret))))) (:shrunk ret))))

(deftest verify-rejects-return-where-throw-expected
  (let [ret (tc/quick-check 100 (c/verify fault-model never-throws-impl))]
    (is (false? (:pass? ret)) ret)))

(deftest test-proxy-handles-throws
  (let [proxy (c/test-proxy fault-model (good-flaky-impl))]
    (is (= :ok (put-item proxy "hello")))
    (is (= :ok (break-network proxy)))
    ;; conforming exception is rethrown to the caller
    (let [e (try (put-item proxy "world") (catch Exception e e))]
      (is (s/valid? ::error (::error (ex-data e))))))

  (let [proxy (c/test-proxy fault-model (never-throws-impl))]
    (is (= :ok (put-item proxy "hello")))
    (is (= :ok (break-network proxy)))
    ;; impl returned where model expected a throw
    (let [e (try (put-item proxy "world") (catch Exception e e))]
      (is (= "implementation did not conform to spec" (ex-message e)))
      (is (= :throw (:expected-outcome (ex-data e))))
      (is (= :return (:impl-outcome (ex-data e))))))

  (let [proxy (c/test-proxy fault-model (undeclared-throw-impl))]
    ;; impl threw where model expected a value; original exception is the cause
    (let [e (try (put-item proxy "hello") (catch Exception e e))]
      (is (= "implementation did not conform to spec" (ex-message e)))
      (is (= :throw (:impl-outcome (ex-data e))))
      (is (= {:cause :disk} (ex-data (ex-cause e)))))))

(deftest test-proxy-model-return-throws
  (let [proxy (c/test-proxy fault-model (good-flaky-impl) :return :model)]
    (is (= :ok (put-item proxy "hello")))
    (is (= :ok (break-network proxy)))
    (let [e (try (put-item proxy "world") (catch Exception e e))]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (s/valid? ::error (::error (ex-data e)))))))

(deftest shrinking-works-with-throws
  (->> (rose/seq (gen/call-gen (c/gen-calls fault-model (p/initial-state fault-model)) (random/make-random) 100))
       (take 500)
       (run! (fn [calls]
               (reduce (fn [state {:keys [method args return]}]
                         (is (p/requires method state) "shrunk state obeys requires")
                         (is (p/precondition method state args) "shrunk state obeys preconditions")
                         (let [ret (p/return method state args)
                               state' (p/next-state ret)]
                           (is (= (c/outcome ret) (c/outcome return)) "shrunk outcome is correct")
                           (is (= state' (p/next-state return)) "shrunk state is correct")
                           state'))
                       (p/initial-state fault-model)
                       calls)))))
