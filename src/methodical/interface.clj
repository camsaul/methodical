(ns methodical.interface
  (:refer-clojure :exclude [isa? prefers prefer-method])
  (:require clojure.core))

;; this is a dummy dependency until Cloverage 1.3.0 is released -- see
;; https://github.com/cloverage/cloverage/issues/318
(comment clojure.core/keep-me)

(defprotocol MethodCombination
  (allowed-qualifiers [method-combination]
    "The set containg all qualifiers supported by this method combination. `nil` in the set means the method
    combination supports primary methods (because primary methods have no qualifier); all other values refer to
    auxiliary methods with that qualifer, e.g. `:before`, `:after`, or `:around`.

    (allowed-qualifiers (clojure-method-combination)) ;-> #{nil}
    (allowed-qualifiers (clos-method-combination))    ;-> #{nil :before :after :around}
    (allowed-qualifiers (doseq-method-combination))   ;-> #{:doseq}")

  (combine-methods [method-combination primary-methods aux-methods]
    "Combine a sequence of matching `primary-methods` with `aux-methods` (a map of qualifier -> sequence of methods)
    into a single effective method. Method includes effective `^:dispatch-value` metadata.")

  (transform-fn-tail [method-combination qualifier fn-tail]
    "Make appropriate transformations to the `fn-tail` of a `defmethod` macro expansion for a primary
    method (qualifier will be `nil`) or an auxiliary method. You can use this method to add implicit args like
    `next-method` to the body of a `defmethod` macro. (Because this method is invoked during macroexpansion, it should
    return a Clojure form.)"))

(defprotocol MethodTable
  (primary-methods [method-table]
    "Get a `dispatch-value -> fn` map of all primary methods assoicated with this method table.")

  (aux-methods [method-table]
    "Get a `qualifier -> dispatch-value -> [fn]` map of all auxiliary methods associated with this method table.")

  (add-primary-method [method-table dispatch-value f]
    "Set the primary method implementation for `dispatch-value`, replacing it if it already exists.")

  (remove-primary-method [method-table dispatch-value]
    "Remove the primary method for `dispatch-value`.")

  (add-aux-method [method-table qualifier dispatch-value f]
    "Add an auxiliary method implementation for `qualifer` (e.g. `:before`) and `dispatch-value`. Unlike primary
    methods, auxiliary methods are not limited to one method per dispatch value; thus this method does not remove
    existing methods for this dispatch value. existing ")

  (remove-aux-method [method-table qualifier dispatch-val method]
    "Remove an auxiliary method from a method table. Because multiple auxiliary methods are allowed for the same
    dispatch value, existing implementations of `MethodTable` are currently only able to remove exact matches -- for
    functions, this usually means identical objects.

    In the future, I hope to fix this by storing unique indentifiers in the metadata of methods in the map."))

(defprotocol Dispatcher
  (dispatch-value
    [dispatcher]
    [dispatcher a]
    [dispatcher a b]
    [dispatcher a b c]
    [dispatcher a b c d]
    [dispatcher a b c d more]
    "Return an appropriate dispatch value for args passed to a multimethod. (This method is equivalent in purpose to
    the dispatch function of vanilla Clojure multimethods.)")

  (matching-primary-methods [dispatcher method-table dispatch-value]
    "Return a sequence of applicable primary methods for `dispatch-value`, sorted from most-specific to
    least-specific. Methods should have the `^:dispatch-value` with which they were defined as metadata. The standard
    dispatcher also checks to make sure methods in the sequence are not ambiguously specific, replacing ambiguous
    methods with ones that will throw an Exception when invoked.")

  (matching-aux-methods [dispatcher method-table dispatch-value]
    "Return a map of aux method qualifier -> sequence of applicable methods for `dispatch-value`, sorted from
    most-specific to least-specific. Methods should have the `^:dispatch-value` with which they were defined as
    metadata.")

  (default-dispatch-value [dispatcher]
    "Default dispatch value to use if no other dispatch value matches.")

  (prefers [dispatcher]
    "Return a map of preferred dispatch value -> set of other dispatch values.")

  (prefer-method [dispatcher dispatch-val-x dispatch-val-y]
    "Prefer `dispatch-val-x` over `dispatch-val-y` for dispatch and method combinations.")

  (dominates? [dispatcher dispatch-val-x dispatch-val-y]
    "Is `dispatch-val-x` considered more specific than `dispatch-val-y`?"))

(defprotocol MultiFnImpl
  (^methodical.interface.MethodCombination method-combination [multifn]
   "Get the method combination associated with this multifn.")

  (^methodical.interface.Dispatcher dispatcher [multifn]
   "Get the dispatcher associated with this multifn.")

  (^methodical.interface.MultiFnImpl with-dispatcher [multifn new-dispatcher]
   "Return a copy of this multifn using `new-dispatcher` as its dispatcher.")

  (^methodical.interface.MethodTable method-table [multifn]
   "Get the method table associated with this multifn.")

  (^methodical.interface.MultiFnImpl with-method-table [multifn new-method-table]
   "Return a copy of this multifn using `new-method-table` as its method table.")

  (effective-method [multifn dispatch-value]
    "Return the effective method for `dispatch-value`. The effective method is a combined primary method and
    applicable auxiliary methods that can be called like a normal function. `effective-method` is similar in purpose
    to `get-method` in vanilla Clojure multimethods; a different name is used here because I felt `get-method` would
    be ambiguous with regards to whether it returns only a primary method or a combined effective method."))

(defprotocol Cache
  (cached-method [cache dispatch-value]
    "Return cached effective method for `dispatch-value`, if it exists in the cache.")

  (cache-method! [cache dispatch-value method]
    "Cache the effective method for `dispatch-value` in this cache.")

  (clear-cache! [cache]
    "Empty the contents of the cache in-place.")

  (^methodical.interface.Cache empty-copy [cache]
   "Return an empty copy of the same type as this cache, e.g. for use when copying a multifn."))
