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

(defn- ->return
  "Shared builder for `return` and `throws`. outcome-kind is :return or
  :throw; spec validates the returned value or the thrown exception
  respectively."
  [outcome-kind spec next-state gen]
  (with-meta
    {:spec spec
     :next-state next-state
     :gen gen}
    {`p/spec (fn [_this] spec)
     `p/next-state (fn [_this] next-state)
     `p/gen (fn [_this] (if gen
                          gen
                          (s/gen spec)))
     `p/outcome (constantly outcome-kind)}))

(defn return
  "Define a return value for a model method. Model methods must always return an instance of `return`.

  spec - a spec or predicate used for validating the implementation's return value
  next state - the model's next state
  gen - A generator used to construct mock return value. If not specified, `spec` must gen
  "
  [spec & {:keys [next-state gen]}]
  (validate! identity spec)
  (validate! (s/nilable gen/generator?) gen)
  (->return :return spec next-state gen))

(defn throws
  "Define a thrown outcome for a model method. Like `return`, but declares
  that the method throws rather than returning a value.

  ex-spec - a spec or predicate used for validating the exception thrown by
  the implementation. See `ex-spec` for the common case of validating an
  ex-info by its ex-data.
  next-state - the model's next state
  gen - a generator constructing exceptions for mocks to throw. If not
  specified, `ex-spec` must gen.

  The model method fn must stay pure: whether a call throws must be a
  function of (state, args) only. Model nondeterministic faults (\"this call
  may time out\") as explicit model state, e.g. a fault flag or queue."
  [ex-spec & {:keys [next-state gen]}]
  (validate! identity ex-spec)
  (validate! (s/nilable gen/generator?) gen)
  (->return :throw ex-spec next-state gen))

(defn outcome
  "Return the outcome kind of a Return value, :return or :throw. Returns
  created before `p/outcome` existed (which neither satisfy the protocol nor
  carry the metadata key) are treated as :return. A `p/outcome`
  implementation that throws is NOT swallowed."
  [ret]
  (if (or (-> ret meta (get `p/outcome))
          (satisfies? p/Return ret))
    (p/outcome ret)
    :return))

(defn ex-spec
  "Returns a spec matching an ex-info whose ex-data conforms to `data-spec`.
  Gens by generating data from `data-spec`, so it can be passed to `throws`
  without an explicit :gen."
  [data-spec]
  (s/with-gen
    (s/and #(instance? #?(:clj clojure.lang.ExceptionInfo
                          :cljs cljs.core/ExceptionInfo) %)
           #(s/valid? data-spec (ex-data %)))
    (fn []
      (gen/fmap (fn [data] (ex-info "mock error" data))
                (s/gen data-spec)))))

(defn- invoke-outcome
  "Call (f impl & args), capturing the outcome as data: {:outcome :return
  :value v} or {:outcome :throw :value exception}. Only Exceptions are
  captured; Errors (OutOfMemory, StackOverflow, AssertionError, ...) are
  left to propagate."
  [f impl args]
  (try
    {:outcome :return :value (apply f impl args)}
    (catch #?(:clj Exception :cljs :default) t
      {:outcome :throw :value t})))

(defn- deliver-outcome
  "Deliver a captured outcome to a caller: return the value, or throw it
  when the outcome kind is :throw."
  [outcome-kind value]
  (if (= :throw outcome-kind)
    (throw value)
    value))

(defn- conforms?
  "True when an implementation's captured outcome matches the model Return's
  expected outcome kind and conforms to its spec."
  [return actual]
  (boolean
   (and return
        (p/spec return)
        (= (outcome return) (:outcome actual))
        (s/valid? (p/spec return) (:value actual)))))

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
  args - (fn [state] -> generator), returns a generator for args to the method. Do not include `this`. Optional; defaults to a generator of no args.
  precondition - (fn [state args] -> bool). Return truthy if it's valid to call this method with these args in the current state."
  [v f & {:keys [requires precondition args]}]
  (validate! var? v)
  (validate! ifn? f)
  (validate! (s/nilable ifn?) args)
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
                                                         (reset! *ret-value {:kind (outcome ret)
                                                                             :value ret-value})
                                                         (p/next-state ret))))
                         ;; deliver outside swap-state: its fn may be retried
                         ;; and must stay side-effect free, so a :throw outcome
                         ;; must not throw from inside it
                         (let [{:keys [kind value]} @*ret-value]
                           (deliver-outcome kind value))))])))
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

(defn gen-call
  "return a generator for a single call to the model"
  [model state]
  (gen/bind (p/gen-method model state)
            (fn [method]
              (gen/fmap (fn [args]
                          {:method method
                           :args args
                           :return (p/return method state args)}) (gen-valid-args state method)))))

(defn gen-calls-
  "Given a model instance, return a sequence of maps containing
  `:method`, `:arguments` and the model's expected return
  spec. Satisfies valid-call-sequence?, but does not shrink properly"
  [model state length]
  (validate! nat-int? length)
  (if (pos? length)
    (gen/gen-bind
      (gen-call model state)
      (fn [call-rose]
        (let [{:keys [return]} (rose/root call-rose)]
          (assert (return? return))
          (let [next-state (p/next-state return)]
            (gen/gen-fmap (fn [rest-calls]
                            (into [call-rose] rest-calls))
                          (gen-calls- model next-state (dec length)))))))
    (gen/gen-pure [])))

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
  "Generate a seq of calls that shrinks properly"
  [model state & {:keys [max-length]
                  :or {max-length 10}}]
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
                            (gen-calls- model state n)))))

(defn test-model
  "Defines a property checking the model. Put it in a defspec"
  [model]
  (validate! ::model model)
  (validate! ::methods (p/methods model))

  (prop/for-all [calls (gen-calls model (p/initial-state model))]
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

  Tests the implementation against a sequence of calls from the model
  "
  [model impl-f & {:keys [num-calls]
                   :or {num-calls 10}
                   :as _opts}]
  (prop/for-all [calls (gen-calls model (p/initial-state model) :max-length num-calls)]
                (let [impl (impl-f)
                      executed-calls (atom [])]
                  (try
                    (every? (fn [call]
                              (swap! executed-calls conj call)
                              (let [{:keys [method args return]} call
                                    actual (invoke-outcome (p/var method) impl args)
                                    ret (conforms? return actual)]
                                (when-not ret
                                  (prn (merge {:fail method
                                               :args args
                                               :expected (p/spec return)
                                               :expected-outcome (outcome return)
                                               :actual-outcome (:outcome actual)}
                                              (if (= :throw (:outcome actual))
                                                {:actual-exception (:value actual)
                                                 :actual-message (ex-message (:value actual))
                                                 :actual-ex-data (ex-data (:value actual))}
                                                {:actual (:value actual)
                                                 :explain (s/explain-data (p/spec return) (:value actual))}))))
                                (swap! executed-calls
                                       (fn [calls call] (-> calls pop (conj call)))
                                       (assoc call
                                              :implementation-return (:value actual)
                                              :implementation-outcome (:outcome actual)))
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
  (validate! #{:implementation :model} return)
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
                                  expected-outcome (outcome @*ret)
                                  actual (invoke-outcome v impl args)]
                              (cond
                                ;; impl threw where the model did not declare a
                                ;; throw: propagate the original exception
                                ;; unchanged, preserving the behavior callers
                                ;; relied on before fault outcomes existed
                                (and (= :throw (:outcome actual))
                                     (not= :throw expected-outcome))
                                (throw (:value actual))

                                (not (conforms? @*ret actual))
                                (throw (ex-info
                                        "implementation did not conform to spec" {:model-ret @*ret
                                                                                  :expected-outcome expected-outcome
                                                                                  :impl-ret (:value actual)
                                                                                  :impl-outcome (:outcome actual)
                                                                                  :explain (s/explain-data ret-spec (:value actual))}
                                        (when (= :throw (:outcome actual))
                                          (:value actual))))

                                (= :implementation return)
                                (deliver-outcome (:outcome actual) (:value actual))

                                :else
                                (deliver-outcome expected-outcome
                                                 (nth (gen/sample (p/gen @*ret)) (rand-int 10)))))))])))
            (into {}))))))
