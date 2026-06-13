(ns griffin.test.contract
  (:refer-clojure :exclude [methods])
  (:require #?@(:clj [[clojure.spec.alpha :as s]]
                :cljs [[cljs.spec.alpha :as s]])
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.rose-tree :as rose]
            [griffin.test.contract.mock-protocol :as mock-protocol]
            [griffin.test.contract.protocol :as p]))

(defn deep-merge
  "Like merge, but merges maps recursively.
  Copied from griffin.util.map"
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn validate!
  ([spec x error-message]
   (if-let [err (s/explain-data spec x)]
     (throw (#?(:clj ex-info :cljs ex-info) error-message err))
     x))
  ([spec x]
   (validate! spec x "value does not conform!")))

(defn return
  "Define a return value for a model method. Model methods must always return an instance of `return`.

  spec - a spec or predicate used for validating the implementation's return value
  next state - the model's next state
  gen - A generator used to construct mock return value. If not specified, `spec` must gen
  "
  [spec & {:keys [next-state gen]}]

  (validate! identity spec)
  (validate! (s/nilable gen/generator?) gen)

  (with-meta
    {:spec spec
     :next-state next-state
     :gen gen}
    {`p/spec (fn [_this] spec)
     `p/next-state (fn [_this] next-state)
     `p/gen (fn [_this] (if gen
                          gen
                          (s/gen spec)))}))

(defn return? [x]
  (or (satisfies? p/Return x)
      (-> x meta (get `p/spec))))

(defn method? [x]
  (or (satisfies? p/Method x)
      (-> x meta (get `p/return))))

(defn method
  "Defines a model protocol method

  v - a protocol method _var_
  f - (Fn [state args] -> `Return`)

  keywords
  requires - (fn [state] -> bool) if provided, return whether it's valid to call this method in the current state. Defaults to `true`
  args - (fn [state] -> generator), returns a generator for args to the method. Do not include `this`
  precondition - (fn [state args] -> bool). Return truthy if it's valid to call this method with these args in the current state."
  [v f & {:keys [requires precondition args]}]
  (validate! var? v)
  (validate! ifn? f)
  (validate! ifn? args)
  (with-meta {:var v}
    {`p/var (constantly v)
     `p/requires (fn [_ state]
                   (if requires
                     (requires state)
                     true))
     `p/args (fn [_ state]
               (if args
                 (args state)
                 (gen/return [])))
     `p/return (fn [_ state args]
                 (f state args))
     `p/precondition (fn [_ state args]
                       (if precondition
                         (precondition state args)
                         true))}))

#?(:clj (defn ref? [x]
          (instance? clojure.lang.Ref x)))

#?(:clj
   (defn ref-state
     "Returns an implementation of MockState that simulates state living longer than the lifetime of a single mock instance (e.g a database client & its backing database).  Pass in a ref that you use to manage the lifetime of mock state"
     [r]
     (validate! ref? r)
     (reify mock-protocol/State
       (mock-protocol/init-state [this i]
         (dosync
          (alter r deep-merge i))
         this)
       (mock-protocol/swap-state [this f]
         (dosync
          (alter r f))
         this))))

(defn ephemeral-state
  "An implementation of MockState that does not persist"
  []
  (let [s (atom nil)]
    (reify mock-protocol/State
      (mock-protocol/init-state [this i]
        (reset! s i)
        this)
      (mock-protocol/swap-state [this f]
        (swap! s f)
        this))))

(defn var->sym [v]
  (symbol (str (-> v meta :ns ns-name)) (str (-> v meta :name))))

(defn mock
  "Given a model, return an instance of the protocol.

  Options:
  mock-state: an instance of `mock-protocol/State`, used to control the lifetime of mock state. See `c/ref-state`
  seed: an RNG seed to pass to `gen/generate`, for deterministic results inside a generative test
  "
  [model & {:keys [mock-state seed]
            :or {mock-state (ephemeral-state)}}]
  (assert (every? :extend-via-metadata (p/protocols model)) ":extend-via-metadata must be set on the protocol")
  (mock-protocol/init-state mock-state (p/initial-state model))
  (with-meta {}
    (->> model
         p/protocols
         (mapcat :method-builders)
         (map (fn [[v _f]]
                (let [s (var->sym v)
                      method (p/get-method model v)]
                  (assert method)
                  [s (fn [_ & args]
                       ;; TODO should this throw if `precondition` is violated?
                       (let [*ret-value (atom nil)]
                         (mock-protocol/swap-state mock-state (fn [current-state]
                                                       (let [ret (p/return method current-state args)
                                                             _ (assert ret)
                                                             ret-spec (p/spec ret)
                                                             _ (assert (p/gen ret) (print-str "p/gen is nil for return value:" ret))
                                                             _ (assert (gen/generator? (p/gen ret)) (print-str "p/gen is not a generator:" ret))
                                                             ret-value (if seed
                                                                         (gen/generate (p/gen ret) 30 seed)
                                                                         (gen/generate (p/gen ret)))]
                                                         (s/assert ret-spec ret-value)
                                                         ;; HACK
                                                         (reset! *ret-value ret-value)
                                                         (p/next-state ret))))
                         @*ret-value))])))
         (into {}))))

(defn model? [x]
  (or (satisfies? p/Model x)
      (-> x meta (get `p/methods))))

(defn default-gen-method [model state]
  (let [valid-methods (->> (p/methods model)
                           (filter (fn [m]
                                     (p/requires m state))))]
    (assert (seq valid-methods) (print-str "At least one command must pass :requires with state: " (prn-str state)))
    (gen/elements valid-methods)))

(defn protocol? [x]
  (and (map? x)
       (:on-interface x)))

(s/def ::model model?)
(s/def ::methods (s/coll-of method?))
(s/def ::protocols (s/coll-of protocol?))

(s/fdef model :args (s/cat :a (s/keys :req-un [::methods ::protocols]
                                      :opt-un [::initial-state ::gen-method ::cleanup])))
(defn model
  "Define a model

  methods - a coll-of `Method`
  protocols - the set of protocols to be tested
  initial-state - (fn [] -> state)
  gen-method - (fn [] -> generator). Optional. If not provided, defaults to a generator that selects from all methods which return `requires` -> true  with uniform probability
  cleanup - `(fn [impl calls] -> any)` Optional.   "
  [& {:keys [methods protocols initial-state gen-method cleanup] :as args}]
  (validate! ::methods methods)
  (validate! ::protocols protocols)

  (let [method-map (into {} (map (fn [m]
                                   [(p/var m) m]) methods))]
    (with-meta
      (select-keys args [:protocols :methods])
      {`p/methods (constantly methods)
       `p/get-method (fn [_ v]
                       (get method-map v))
       `p/protocols (constantly protocols)
       `p/initial-state (fn [_]
                          (when initial-state
                            (initial-state)))
       `p/gen-method (fn [this state]
                       (if gen-method
                         (gen-method state)
                         (default-gen-method this state)))
       `p/cleanup (fn [_this impl calls]
                    (when cleanup
                      (cleanup impl calls)))})))

(defn gen-valid-args [state method]
  (let [gen-args (p/args method state)]
    (validate! gen/generator? gen-args (str ":args must return a generator for " method))
    (gen/such-that (fn [args] (p/precondition method state args))
                   gen-args)))

;; --- Swarm testing (Groce et al., ISSTA 2012) ---
;;
;; With uniform method selection, every generated sequence draws from the full
;; set of methods, so "interesting" sequences (e.g. many `set`s in a row with no
;; `get`/`delete`) are vanishingly unlikely. Swarm testing instead picks a random
;; *subset* ("config") of the model's methods once per sequence and generates the
;; whole sequence from only that subset. The diversity - especially the omission
;; of methods in some runs - surfaces bugs uniform generation effectively never
;; reaches.

(def default-swarm-opts
  "Default swarm options. See `gen-swarm-config`."
  {:probability 0.5 :min-size 1 :always #{}})

(defn resolve-swarm-opts
  "Normalize the `:swarm` option into an options map, or nil when swarm is off.
  Accepts nil/false (off), true (defaults), or a (partial) options map."
  [swarm]
  (cond
    (or (nil? swarm) (false? swarm)) nil
    (true? swarm) default-swarm-opts
    (map? swarm) (merge default-swarm-opts swarm)
    :else (throw (#?(:clj ex-info :cljs ex-info)
                  ":swarm must be true, false, nil, or a map of options"
                  {:swarm swarm}))))

(defn- gen-include?
  "A generator of a boolean that is true with probability `p`. Safe at the
  extremes p<=0 and p>=1 (where `gen/frequency` would otherwise reject a zero
  weight)."
  [p]
  (cond
    (<= p 0) (gen/return false)
    (>= p 1) (gen/return true)
    :else (gen/frequency [[(int (* 1000 p)) (gen/return true)]
                          [(int (* 1000 (- 1 p))) (gen/return false)]])))

(defn gen-swarm-config
  "Return a generator of a swarm config: a non-empty set of method vars drawn
  from the model's methods. Each method is independently included with
  probability `:probability`; method vars in `:always` are always included; and
  the result is topped up deterministically to at least `:min-size` methods.

  The config is drawn once per call sequence (see `gen-calls`)."
  [model swarm-opts]
  (let [{:keys [probability min-size always]} (merge default-swarm-opts swarm-opts)
        all-vars (mapv p/var (p/methods model))
        always (set always)
        optional (vec (remove always all-vars))
        min-size (max 1 (min min-size (count all-vars)))]
    (gen/fmap
     (fn [flags]
       (let [picked (into always
                          (keep-indexed (fn [i v] (when (nth flags i) v)) optional))]
         ;; top up to :min-size from the not-yet-picked methods
         (loop [s picked
                pool (remove s optional)]
           (if (or (>= (count s) min-size) (empty? pool))
             s
             (recur (conj s (first pool)) (rest pool))))))
     (if (seq optional)
       (apply gen/tuple (map (fn [_] (gen-include? probability)) optional))
       (gen/return [])))))

(defn swarm-gen-method
  "Like `default-gen-method`, but only selects from methods whose var is in
  `config` (a set of method vars). If no method in `config` passes `requires`
  for the current `state`, falls back to the full set of valid methods, so
  generation never deadlocks. Preserves the existing `requires` invariant: it
  only asserts when *no* method is valid (a genuine model bug, swarm or not)."
  [model config state]
  (let [valid (->> (p/methods model)
                   (filter (fn [m] (p/requires m state))))
        in-config (filter (fn [m] (contains? config (p/var m))) valid)]
    (assert (seq valid) (print-str "At least one command must pass :requires with state: " (prn-str state)))
    (gen/elements (if (seq in-config) in-config valid))))

(defn gen-call
  "return a generator for a single call to the model.

  `config`, when non-nil, is a swarm config (a set of method vars) that
  restricts which method may be selected for this call."
  ([model state] (gen-call model state nil))
  ([model state config]
   (gen/bind (if config
               (swarm-gen-method model config state)
               (p/gen-method model state))
             (fn [method]
               (gen/fmap (fn [args]
                           {:method method
                            :args args
                            :return (p/return method state args)}) (gen-valid-args state method))))))

(defn gen-calls-
  "Given a model instance, return a sequence of maps containing
  `:method`, `:arguments` and the model's expected return
  spec. Satisfies valid-call-sequence?, but does not shrink properly"
  ([model state length] (gen-calls- model state length nil))
  ([model state length config]
   (validate! nat-int? length)
   (if (pos? length)
     (gen/gen-bind
       (gen-call model state config)
       (fn [call-rose]
         (let [{:keys [return]} (rose/root call-rose)]
           (assert (return? return))
           (let [next-state (p/next-state return)]
             (gen/gen-fmap (fn [rest-calls]
                             (into [call-rose] rest-calls))
                           (gen-calls- model next-state (dec length) config))))))
     (gen/gen-pure []))))

(defn recompute-state
  "Given a seq of calls that has been modified, recompute state. Returns the calls or nil if a precondition failed"
  [model calls]
  (->> calls
       (reduce (fn [{:keys [calls state]} {:keys [args method] :as _call}]
                 (if (and (p/requires method state)
                          (p/precondition method state args))
                   (let [ret (p/return method state args)]
                     {:calls (conj calls {:method method
                                          :args args
                                          :return ret})
                      :state (p/next-state ret)})
                   (reduced nil)))
               {:calls []
                :state (p/initial-state model)})
       :calls))

(defn gen-calls
  "Generate a seq of calls that shrinks properly.

  Options:
  max-length - maximum number of calls to generate (default 10)
  swarm - enable swarm testing. nil/false (default, off), true (defaults), or a
          map of options (see `gen-swarm-config`). When enabled, a random subset
          of the model's methods is chosen once per sequence and the whole
          sequence is generated from only that subset."
  [model state & {:keys [max-length swarm]
                  :or {max-length 10}}]
  (let [swarm-opts (resolve-swarm-opts swarm)
        config-gen (if swarm-opts
                     (gen-swarm-config model swarm-opts)
                     (gen/return nil))]
    (gen/bind
     config-gen
     (fn [config]
       ;; the swarm config is bound once here, so it is fixed for the whole
       ;; generated sequence (and does not shrink - shrinking only removes or
       ;; simplifies the calls already drawn from the config).
       (gen/bind (gen/large-integer* {:min 1 :max max-length})
                 (fn [n]
                   (gen/gen-fmap (fn [rose-calls]
                                   ;; http://blog.guillermowinkler.com/blog/2015/04/12/verifying-state-machine-behavior-using-test-dot-check/
                                   ;; A rose tree holds a 'real' value at the root, with children being
                                   ;; possible shrinks. Normal vectors shrink both by shrinking each
                                   ;; item and by removing items (one at a time or half at once)
                                   ;; We lazily recompute the state for each potential shrunk vector
                                   ;; of calls, and remove any that are not valid (which also removes
                                   ;; all its children), to guarantee test.check only picks valid ones.

                                   (->> (rose/shrink-vector vector rose-calls)
                                        (rose/fmap #(recompute-state model %))
                                        (rose/filter some?)))
                                 (gen-calls- model state n config))))))))

(defn test-model
  "Defines a property checking the model. Put it in a defspec.

  swarm - enable swarm testing (see `gen-calls`). Defaults to off."
  [model & {:keys [swarm]}]
  (validate! ::model model)
  (validate! ::methods (p/methods model))

  (prop/for-all [calls (gen-calls model (p/initial-state model) :swarm swarm)]
                (->> calls
                     (every? (fn [c]
                               (and c
                                    (:return c)
                                    (p/gen (:return c))
                                    (doall (gen/sample (p/gen (:return c))))))))))

(defn verify
  "Verify an implementation of a protocol against a model. Returns a _property_, put it in a `defspec`

  impl-f: a no argument constructor of the implementation

  num-calls: maximum call length to generate in a single run

  swarm: enable swarm testing (see `gen-calls`). Defaults to off.

  Tests the implementation against a sequence of calls from the model
  "
  [model impl-f & {:keys [num-calls swarm]
                   :or {num-calls 10}
                   :as _opts}]
  (prop/for-all [calls (gen-calls model (p/initial-state model) :max-length num-calls :swarm swarm)]
                (let [impl (impl-f)
                      executed-calls (atom [])]
                  (try
                    (every? (fn [call]
                              (swap! executed-calls conj call)
                              (let [{:keys [method args return]} call
                                    impl-ret (apply (p/var method) impl args)
                                    ret (and return
                                             (p/spec return)
                                             (s/spec (p/spec return))
                                             (s/valid? (p/spec return) impl-ret))]
                                (when-not ret
                                  (prn {:fail method
                                        :args args
                                        :expected (p/spec return)
                                        :actual impl-ret
                                        :explain (s/explain-data (p/spec return) impl-ret)}))
                                (swap! executed-calls
                                       (fn [calls call] (-> calls pop (conj call)))
                                       (assoc call :implementation-return impl-ret))
                                ret)) calls)
                    (finally
                      (p/cleanup model impl @executed-calls))))))

(defn test-proxy
  "Given a model and an implementation, return a new implementation of the
  protocol that passes all calls to the implementation, checking
  return values against the model. Function calls will throw when the
  implementation return value does not conform to the model return
  spec.  Prefer this in all non-contract tests to identify
  discrepancies between the model and implementation.

  Options:
  - return: `:model`, `:implementation`. Selects which return value to return to the caller. defaults to :implementation.
  "
  [model impl & {:keys [return mock-state]
                 :or {return :implementation
                      mock-state (ephemeral-state)}}]
  (validate! ::model model)
  (let [state (mock-protocol/init-state mock-state (p/initial-state model))]
    (with-meta {}
      (merge
       (->> model
            p/protocols
            (mapcat :method-builders)
            (map (fn [[v _f]]
                   (let [s (var->sym v)
                         method (p/get-method model v)]
                     [s (fn [_this & args]
                          (let [*ret (atom nil)]
                            (mock-protocol/swap-state state (fn [current-state]
                                                     (let [ret (p/return method current-state args)]
                                                       (reset! *ret ret)
                                                       (p/next-state ret))))
                            (let [ret-spec (p/spec @*ret)
                                  impl-ret (apply v impl args)]
                              (when (not (s/valid? ret-spec impl-ret))
                                (throw (ex-info
                                        "implementation did not conform to spec" {:model-ret @*ret
                                                                                  :impl-ret impl-ret
                                                                                  :explain (s/explain-data ret-spec impl-ret)})))
                              (if (= :implementation return)
                                impl-ret
                                (nth (gen/sample (p/gen @*ret)) (rand-int 10))))))])))
            (into {}))))))
