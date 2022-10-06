(ns methodical.core
  "Combined interface to everything in Methodical you'd normally want to use."
  (:refer-clojure :exclude [defmulti defmethod methods get-method remove-method
                            remove-all-methods prefer-method prefers])
  (:require methodical.impl
            methodical.interface
            methodical.macros
            methodical.util
            methodical.util.describe
            methodical.util.dispatch
            methodical.util.trace
            [potemkin :as p]))

;; fool cljr-clean-ns and the namespace linter so it doesn't remove these automatically
(comment
  methodical.impl/keep-me
  methodical.interface/keep-me
  methodical.macros/keep-me
  methodical.util.describe/keep-me
  methodical.util.dispatch/keep-me
  methodical.util.trace/keep-me
  methodical.util/keep-me)

(p/import-vars
 [methodical.macros
  defmulti
  defmethod]

 [methodical.interface
  ;; MethodCombination
  allowed-qualifiers
  combine-methods
  transform-fn-tail
  ;; MethodTable
  primary-methods
  add-primary-method
  remove-primary-method
  add-aux-method
  remove-aux-method
  ;; Dispatcher
  default-dispatch-value
  prefers
  with-prefers
  dominates?
  ;; MultiFnImpl
  method-combination
  dispatcher
  with-dispatcher
  method-table
  with-method-table
  effective-method]

 [methodical.impl
  ;; method combinations
  clojure-method-combination
  clos-method-combination
  thread-first-method-combination
  thread-last-method-combination
  do-method-combination
  min-method-combination
  max-method-combination
  +-method-combination
  seq-method-combination
  concat-method-combination
  and-method-combination
  or-method-combination
  ;; dispatchers
  standard-dispatcher
  everything-dispatcher
  multi-default-dispatcher
  ;; method tables
  clojure-method-table
  standard-method-table
  ;; caches
  simple-cache
  watching-cache
  ;; multifn impls
  standard-multifn-impl
  cached-multifn-impl
  default-multifn-impl
  clojure-multifn-impl
  clos-multifn-impl
  ;; multifn
  uncached-multifn
  multifn
  default-multifn]

 [methodical.util
  add-aux-method-with-unique-key
  applicable-primary-method
  aux-methods
  default-aux-methods
  default-effective-method
  default-primary-method
  dispatch-fn
  dispatch-value
  effective-dispatch-value
  effective-primary-method
  is-default-effective-method?
  is-default-primary-method?
  matching-aux-methods
  matching-primary-methods
  prefer-method
  primary-method
  remove-all-aux-methods
  remove-all-aux-methods-for-dispatch-val
  remove-all-methods
  remove-all-preferences
  remove-all-primary-methods
  remove-aux-method-with-unique-key
  unprefer-method
  ;; destructive ops
  add-aux-method!
  add-aux-method-with-unique-key!
  add-primary-method!
  prefer-method!
  remove-all-aux-methods!
  remove-all-aux-methods-for-dispatch-val!
  remove-all-methods!
  remove-all-preferences!
  remove-all-primary-methods!
  remove-aux-method!
  remove-aux-method-with-unique-key!
  remove-primary-method!
  unprefer-method!
  with-prefers!]

 [methodical.util.describe
  describe]

 [methodical.util.dispatch
  dispatch-on-first-arg
  dispatch-on-first-two-args
  dispatch-on-first-three-args
  dispatch-on-first-four-args]

 [methodical.util.trace
  trace])
