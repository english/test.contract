Contract testing for Clojure

# Motivation

Integration tests are brittle, slow and error prone. Conventionally, when integrating with external side effects, like a 3rd party API or a networked database, users are presented with a choice: either all tests run against the real database or API which is slow or unreliable, or using mocks. Mocks are fast and deterministic, but there's no guarantee that the mock behavior corresponds to reality.

If you have a large number of tests which run against a slow external service, odds are the vast majority of the tests aren't specifically about the external service, but they depend on it for behavior.

Contracts are the solution. A contract is a generative test suite against the caller and implementation of protocol, covering inputs, outputs and state mutations over time. All implementations may be tested against the contract, and mocks can be automatically built from the contract.

Use contracts to unify the behavior of integration tests and mocks, while decoupling tests. Behavior for both sides is specified in one place. Write integration tests against the real service in one place, and test that thoroughly. The rest of the system then uses a mock which is guaranteed to uphold the guarantees which were specified.

# Goals

- significantly reduce the number of integration tests in a test suite
- fast, reliable, deterministic unit and integration tests
- ability to deterministically introduce faults into the system while testing

# Design

A model describes the protocol under test. It describes the methods that can be called, the arguments to use for those methods, and the state transitions that result from calling the function.

The model is used to generate a sequence of calls against the protocol. The calls are run against the implementation under test, and specs returned by the model are used to validate the implementation return values.

# Walkthrough

```clojure
(defprotocol RemoteAPI
  :extend-via-metadata true
  (create-file [this file]))
```

Define a protocol as normal. Because of how mocks are constructed, `:extend-via-metadata` must be true.

In our example, let's say it's an error to attempt to create a file twice.

Define a model of the protocol:

```clojure
(def model
  (c/model
   {:protocols #{RemoteAPI}
    :methods [(c/method
               #'create-file
               (fn [state [file]]
                 (if (not (get-in state [:files file]))
                   (c/return (gs/constant :ok)
                            :next-state (update state :files conj file))
                   (c/return (gs/constant :error/file-exists)
                            :next-state state)))
               :args (fn [_state] (gen/tuple gen/string)))]
    :initial-state (fn [] {:files #{}})}))
```

`:methods` is a coll-of method definitions. `c/method`'s first argument is a method var, and the second is `(fn [state args] -> contract/return)`. `contract/return` specifies the spec/predicate used to test implementations, and optionally also mutates state and defines the generator for mock return values. `:args` is used to return a generator for arguments to the method given the current state.

`verify` takes the model, and a no-arg constructor for the IUT and runs a generative test

```clojure
(deftest remote-api-contracts
  (c/verify model ->RealRemoteAPIClient))
=>
{:result true,
 :pass? true,
 :num-tests 100,
 :time-elapsed-ms 115823,
 :seed 1664457195393}
```

Run it in your test suite for each implementation.

# Mocking

```clojure
(c/mock model)
```

Returns an instance of the protocol based on the model definition. Return values will be generated from the correponding method return `:gen` or `:spec`.

For all other tests in your suite which require an instance of the protocol, prefer using the mock.

# Test Proxy

```clojure
(c/test-proxy model impl)
```

Returns a new instance of the protocol that passes calls to the 'real' implementation, and compares the implementation's return value against the model. Throws when the implementation and model disagree.

Prefer using it in integration tests to identify discrepancies between the model and the implementation.

## Preconditions and Shrinking

A significant part of the contracts API deals with preconditions and whether it is "valid" to make a call. Clearly other users can call any protocol method at any time, so this is mostly about interestingness and test coverage, rather than correctness.

Consider a hypothetical API that takes IDs as an argument:

```clojure
(create 1)
(create 2)
(delete 2)
```

By default, if a property doesn't hold, test.check shrinks the inputs randomly, so one possible shrunk call sequence is:

```clojure
(create 1)
(delete 2)
```

This is not an _interesting_ case, but it is _possible_, in the sense that a user could make that sequence, and _something_ will happen. In a stateful system, the search space is exponentially larger than a stateless system (every new bit of implicit state multiplies the possible states by 2). We use preconditions to prune the search space to cases that are interesting.

## Explicit State

To make shrinking work, the model state must be a single immutable value, passed from `init-state` to `next-state`. Consider a similar example as above

```clojure
(create 1)
(create 2)
(delete 1)
```
which this time, due to precondition checks, shrinks to

```clojure
(create 1)
(delete 1)
```

When we shrink, we are effectively "rewinding" state. The call to `(create 2)` must no longer be present in state during the shrunk call sequence. Therefore, the state must be an immutable value, and we pass it in to the next generated call.

The corollary here is that the model must not close over any state which mutates during a test.

# Swarm Testing

By default, each generated call sequence draws from *all* of the model's methods with uniform probability. This means "interesting" sequences are vanishingly unlikely: in a cache modeled with `set`/`get`/`delete`, a run of ten `set`s in a row (which might trigger an eviction bug) essentially never occurs, because `get` and `delete` keep getting mixed in.

Swarm testing ([Groce et al., ISSTA 2012](https://agroce.github.io/issta12.pdf)) addresses this. When enabled, a random *subset* ("config") of the model's methods is chosen once per call sequence, and the whole sequence is generated from only that subset. Because some methods are omitted entirely in any given run, you get long runs of the remaining methods - exactly the sequences uniform generation misses.

Enable it by passing `:swarm` to `verify` (or `test-model`):

```clojure
(c/verify model ->RealRemoteAPIClient :swarm true)
```

`:swarm` accepts `true` (defaults), `false`/`nil` (off, the default), or a map of options:

```clojure
(c/verify model ->RealRemoteAPIClient
          :swarm {:probability 0.5        ; per-method inclusion probability (default 0.5)
                  :min-size    1          ; minimum methods per config (default 1)
                  :always      #{#'create-file}}) ; method vars always included (default #{})
```

- `:probability` - lower values produce smaller configs and longer single-method runs.
- `:min-size` - a floor on config size (clamped to the number of methods).
- `:always` - method vars that appear in every config. Useful for a "constructor" method (e.g. `create-file`) that other methods depend on via `:requires`.

If a chosen config excludes every method that is valid (per `:requires`) in some reachable state, generation falls back to the full set of valid methods for that step, so swarm never deadlocks. Swarm only affects generation; shrinking is unaffected and still produces minimal, valid counterexamples.

# Limitations



# Injecting Errors

Coming Soon

# Cleaning up

If the model supplies `:cleanup`, `(fn [state impl calls])`, it will be called at the end of `verify`. Use it to clean up e.g. real resources created during the test.

# Usage
Test.contract is not intended to completely replace all other testing strategies, nor be the sole source of testing for a protocol implementation. It is intended to provide confidence that callers and implementers of a protocol agree on what the specific behavior is. A good heuristic for asking whether a behavior should be included in the contract is to ask "will tests that consume the protocol break if this detail is not specified?". If not, leave it out.

# Acknowledgements

Significant inspiration from:

- http://quviq.com/documentation/eqc/eqc_statem.html
- http://blog.guillermowinkler.com/blog/2015/04/12/verifying-state-machine-behavior-using-test-dot-check/
- [czan/stateful-check](https://github.com/czan/stateful-check)
