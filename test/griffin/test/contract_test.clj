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
    ;; Use enough iterations that generation reliably selects the method with
    ;; the broken :args; with `quick-check 1` the test was flaky because a
    ;; single short run may never exercise file-exists?.
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":args must return a generator for*"
         (tc/quick-check 100 (c/test-model broken-model))))))

;; ---- Swarm testing ----

(deftest swarm-opts-resolution
  (is (nil? (c/resolve-swarm-opts nil)) "nil -> off")
  (is (nil? (c/resolve-swarm-opts false)) "false -> off")
  (is (= c/default-swarm-opts (c/resolve-swarm-opts true)) "true -> defaults")
  (is (= 0.1 (:probability (c/resolve-swarm-opts {:probability 0.1}))) "map override")
  (is (= 1 (:min-size (c/resolve-swarm-opts {:probability 0.1}))) "map merges defaults")
  (is (thrown? Exception (c/resolve-swarm-opts 5)) "invalid -> throws"))

(deftest swarm-config-properties
  (doseq [opts [{} {:probability 0.0} {:probability 1.0} {:min-size 2} {:min-size 3}
                {:min-size 99} {:probability 0.0 :always #{#'create-file}}]]
    (testing opts
      (let [configs  (gen/sample (c/gen-swarm-config model opts) 100)
            all-vars (set (map p/var (p/methods model)))
            min-size (max 1 (min (get opts :min-size 1) (count all-vars)))
            always   (set (:always opts))]
        (doseq [cfg configs]
          (is (set? cfg) "config is a set")
          (is (seq cfg) "config is non-empty")
          (is (every? all-vars cfg) "config is a subset of the model's method vars")
          (is (>= (count cfg) min-size) "config respects :min-size")
          (is (every? cfg always) "config includes every :always method"))))))

(deftest swarm-verify-works
  (is (:pass? (tc/quick-check 100 (c/verify model good-impl :swarm true)))))

(deftest swarm-verify-catches-errors
  (let [ret (tc/quick-check 100 (c/verify model bad-impl :swarm true))]
    (is (false? (:pass? ret)) ret)))

(deftest swarm-off-works-like-default
  (is (:pass? (tc/quick-check 50 (c/verify model good-impl :swarm false))))
  (is (:pass? (tc/quick-check 50 (c/verify model good-impl :swarm nil)))))

(deftest swarm-produces-homogeneous-runs
  ;; With a forced single-method config, every generated sequence is composed
  ;; entirely of that one method - long single-method runs that uniform
  ;; generation effectively never produces (this is the whole point of swarm).
  (let [seqs (gen/sample (c/gen-calls model (p/initial-state model)
                                      :max-length 12
                                      :swarm {:always #{#'create-file} :probability 0.0})
                         100)]
    (is (every? (fn [calls]
                  (every? #(= #'create-file (p/var (:method %))) calls))
                seqs)
        "every swarm call sequence uses only create-file")
    (is (some #(>= (count %) 6) seqs)
        "swarm produces long single-method runs"))
  ;; Uniform generation mixes methods: at least one sequence uses >1 distinct method.
  (let [seqs (gen/sample (c/gen-calls model (p/initial-state model) :max-length 12)
                         100)]
    (is (some (fn [calls]
                (> (count (distinct (map #(p/var (:method %)) calls))) 1))
              seqs)
        "uniform generation mixes multiple methods")))

(deftest swarm-fallback-avoids-deadlock
  ;; A config of only delete-file (which is invalid in the initial state, since
  ;; :requires needs a non-empty :files) must not deadlock generation:
  ;; swarm-gen-method falls back to a valid method to make progress, then uses
  ;; delete-file once it becomes valid. Generation completing (no throw) and
  ;; every sequence being valid proves the hazard is handled.
  (let [seqs (gen/sample (c/gen-calls model (p/initial-state model)
                                      :max-length 10
                                      :swarm {:always #{#'delete-file} :probability 0.0})
                         50)]
    (is (seq seqs))
    (doseq [calls seqs]
      (reduce (fn [state {:keys [method args]}]
                (is (p/requires method state) "fallback obeys requires")
                (is (p/precondition method state args) "fallback obeys preconditions")
                (p/next-state (p/return method state args)))
              (p/initial-state model)
              calls))))

(deftest swarm-shrinking-produces-valid-calls
  ;; Same invariant as shrinking-produces-valid-calls, but with swarm enabled:
  ;; shrinking still only produces valid call sequences (recompute-state is
  ;; unaffected by the swarm config).
  (->> (rose/seq (gen/call-gen (c/gen-calls model (p/initial-state model) :swarm true)
                               (random/make-random 42) 100))
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

;; A protocol with a bug that only manifests after several consecutive
;; cache-set calls with no intervening cache-get - the canonical example of a
;; bug uniform random generation misses but swarm reliably finds.

(defprotocol Cache
  :extend-via-metadata true
  (cache-set [this k v])
  (cache-get [this k]))

(def cache-model
  (c/model
   {:protocols #{Cache}
    :methods [(c/method #'cache-set
                        (fn [state [k v]]
                          (c/return #{:ok}
                                    :next-state (assoc-in state [:cache k] v)))
                        :args (fn [_state] (gen/tuple gen/keyword gen/nat)))
              (c/method #'cache-get
                        (fn [state [_k]]
                          (c/return #{:ok}
                                    :next-state state))
                        :args (fn [_state] (gen/tuple gen/keyword)))]
    :initial-state (fn [] {:cache {}})}))

(defn buggy-cache
  "Returns :ok normally, but breaks (returns :error) after more than 3
  consecutive cache-set calls with no intervening cache-get."
  []
  (let [consecutive (atom 0)]
    (reify Cache
      (cache-set [_ _ _]
        (if (> (swap! consecutive inc) 3)
          :error
          :ok))
      (cache-get [_ _]
        (reset! consecutive 0)
        :ok))))

(deftest swarm-catches-consecutive-call-bug
  ;; Swarm isolates cache-set into long runs and reliably triggers the bug.
  (let [ret (tc/quick-check 100
                            (c/verify cache-model buggy-cache
                                      :num-calls 12
                                      :swarm {:always #{#'cache-set} :probability 0.0}))]
    (is (false? (:pass? ret)) ret)
    ;; the minimal failing case is 4 consecutive cache-set calls
    (is (= 4 (count (first (:smallest (:shrunk ret)))))
        (:shrunk ret))))
