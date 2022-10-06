(ns methodical.interface
  (:refer-clojure :exclude [isa? prefers])
  (:require clojure.core))

(set! *warn-on-reflection* true)

;; this is a dummy dependency until Cloverage 1.3.0 is released -- see
;; https://github.com/cloverage/cloverage/issues/318
(comment clojure.core/keep-me)

(defprotocol MethodCombination
  "A *method combination* defines the way applicable primary and auxiliary methods are combined into a single *effective
  method*. Method combinations also specify which auxiliary method *qualifiers* (e.g. `:before` or `:around`) are
  allowed, and how `defmethod` macro forms using those qualifiers are expanded (e.g., whether they get an implicit
  `next-method` arg)."
  (allowed-qualifiers [method-combination]
    "The set containing all qualifiers supported by this method combination. `nil` in the set means the method
  combination supports primary methods (because primary methods have no qualifier); all other values refer to auxiliary
  methods with that qualifier, e.g. `:before`, `:after`, or `:around`.

  ```clj
  (allowed-qualifiers (clojure-method-combination)) ;-> #{nil}
  (allowed-qualifiers (clos-method-combination))    ;-> #{nil :before :after :around}
  (allowed-qualifiers (doseq-method-combination))   ;-> #{:doseq}
  ```")

  (combine-methods [method-combination primary-methods aux-methods]
    "Combine a sequence of matching `primary-methods` with `aux-methods` (a map of qualifier -> sequence of methods)
  into a single effective method. Method includes effective `^:dispatch-value` metadata.")

  (transform-fn-tail [method-combination qualifier fn-tail]
    "Make appropriate transformations to the `fn-tail` of a [[methodical.macros/defmethod]] macro expansion for a
  primary method (qualifier will be `nil`) or an auxiliary method. You can use this method to add implicit args like
  `next-method` to the body of a `defmethod` macro. (Because this method is invoked during macroexpansion, it should
  return a Clojure form.)"))

(defprotocol MethodTable
  "A *method table* stores primary and auxiliary methods, and returns them when asked. The default implementation,
   [[methodical.impl/standard-method-table]], uses simple Clojure immutable maps."
  (primary-methods [method-table]
    "Get a `dispatch-value -> fn` map of all primary methods associated with this method table.")

  (aux-methods [method-table]
    "Get a `qualifier -> dispatch-value -> [fn]` map of all auxiliary methods associated with this method table.")

  (add-primary-method ^methodical.interface.MethodTable [method-table dispatch-value f]
    "Set the primary method implementation for `dispatch-value`, replacing it if it already exists.")

  (remove-primary-method ^methodical.interface.MethodTable [method-table dispatch-value]
    "Remove the primary method for `dispatch-value`.")

  (add-aux-method ^methodical.interface.MethodTable [method-table qualifier dispatch-value f]
    "Add an auxiliary method implementation for `qualifier` (e.g. `:before`) and `dispatch-value`. Unlike primary
    methods, auxiliary methods are not limited to one method per dispatch value; thus this method does not remove
    existing methods for this dispatch value.")

  (remove-aux-method ^methodical.interface.MethodTable [method-table qualifier dispatch-val method]
    "Remove an auxiliary method from a method table. Because multiple auxiliary methods are allowed for the same
    dispatch value, existing implementations of `MethodTable` are currently only able to remove exact matches -- for
    functions, this usually means identical objects.

    In the future, I hope to fix this by storing unique indentifiers in the metadata of methods in the map."))

(defprotocol Dispatcher
  "A *dispatcher* decides which dispatch value should be used for a given set of arguments, which primary and
  auxiliary methods from the *method table* are applicable for that dispatch value, and the order those methods should
  be applied in -- which methods are most specific, and which are the least specific (e.g., `String` is more-specific
  than `Object`)."
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

  ;; TODO -- consider whether this should be renamed `preferences` -- seems a little clearer
  (prefers [dispatcher]
    "Return a map of preferred dispatch value -> set of other dispatch values.")

  ;; TODO -- consider renaming this to `with-preferences`
  (^methodical.interface.Dispatcher ^{:style/indent :defn} with-prefers [dispatcher new-prefs]
    "Return a copy of `dispatcher` with its preferences map replaced with `new-prefs`.")

  (dominates? [dispatcher dispatch-val-x dispatch-val-y]
    "Is `dispatch-val-x` considered more specific than `dispatch-val-y`?"))

(defprotocol MultiFnImpl
  "Protocol for a complete Methodical multimethod, excluding the optional cache (multimethods with caching wrap a
  `MultiFnImpl`). Methodical multimethods are divided into four components: a *method combination*, which
  implements [[methodical.interface/MethodCombination]]; a *method table*, which
  implements [[methodical.interface/MethodTable]]; a *dispatcher*, which
  implements [[methodical.interface/Dispatcher]]; and, optionally, a *cache*, which
  implements [[methodical.interface/Cache]]. The methods in *this* protocol are used to access or modify the various
  constituent parts of a methodical multimethod, and to use them in concert to create an *effective method*."
  (^methodical.interface.MethodCombination method-combination [multifn]
   "Get the method combination associated with this multifn.")

  (^methodical.interface.Dispatcher dispatcher [multifn]
   "Get the dispatcher associated with this multifn.")

  (^methodical.interface.MultiFnImpl ^{:style/indent :defn} with-dispatcher [multifn new-dispatcher]
   "Return a copy of this multifn using `new-dispatcher` as its dispatcher.")

  (^methodical.interface.MethodTable method-table [multifn]
   "Get the method table associated with this multifn.")

  (^methodical.interface.MultiFnImpl ^{:style/indent :defn} with-method-table [multifn new-method-table]
   "Return a copy of this multifn using `new-method-table` as its method table.")

  (effective-method [multifn dispatch-value]
    "Return the effective method for `dispatch-value`. The effective method is a combined primary method and applicable
    auxiliary methods that can be called like a normal function. [[effective-method]] is similar in purpose
    to [[clojure.core/get-method]] in vanilla Clojure multimethods; a different name is used here because I felt
    `get-method` would be ambiguous with regards to whether it returns only a primary method or a combined effective
    method."))

(defprotocol Cache
  "A *cache*, if present, implements a caching strategy for effective methods, so that they need not be recomputed on
  every invocation."
  (cached-method [cache dispatch-value]
    "Return cached effective method for `dispatch-value`, if it exists in the cache.")

  (cache-method! [cache dispatch-value method]
    "Cache the effective method for `dispatch-value` in this cache.")

  (clear-cache! [cache]
    "Empty the contents of the cache in-place.")

  (^methodical.interface.Cache empty-copy [cache]
   "Return an empty copy of the same type as this cache, e.g. for use when copying a multifn."))
