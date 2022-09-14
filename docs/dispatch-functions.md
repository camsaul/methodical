# Dispatch function utilities

Methodical ships with a few of helper functions for creating common dispatch functions:

* [[methodical.util.dispatch/dispatch-on-first-arg]]
* [[methodical.util.dispatch/dispatch-on-first-two-args]]
* [[methodical.util.dispatch/dispatch-on-first-three-args]]
* [[methodical.util.dispatch/dispatch-on-first-four-args]]

For your convenience, these are also aliased in [[methodical.core]].

These methods all take a `dispatch-fn` argument and return a new function that will call `(dispatch-fn x)` with each
of the first `n` arguments, and ignore all other arguments, for example:

```clj
(def dispatch-fn (m/dispatch-on-first-arg keyword))

(dispatch-fn "a")
;; => (keyword a)
;; => :a
(dispatch-fn "a" "b")
;; => :a
(dispatch-fn "a" "b" "c")
;; => :a

(def dispatch-fn-2 (m/dispatch-on-first-two-args keyword))

(dispatch-fn "a" "b")
;; => [(keyword "a") (keyword "b")]
;; => [:a :b]
(dispatch-fn "a" "b" "c")
;; => [:a :b]
```

[[methodical.util.dispatch/dispatch-on-first-two-args]], [[methodical.util.dispatch/dispatch-on-first-three-args]],
and [[methodical.util.dispatch/dispatch-on-first-four-args]] all support an additional arity that allows you to pass a
separate function to use for each argument, e.g.

```clj
(def dispatch-fn (m/dispatch-on-first-two-args keyword str))

(dispatch-fn "a" "b")
;; => [(keyword "a") (str "b")]
;; => [:a "b"]
(dispatch-fn "a" "b" "c")
;; => [:a "b"]
```
