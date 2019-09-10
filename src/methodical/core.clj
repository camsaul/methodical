(ns methodical.core
  "Combined interface to everything in Methodical you'd normally want to use."
  (:refer-clojure
   :exclude
   [defmulti
    defmethod
    methods
    get-method
    remove-method
    remove-all-methods
    prefer-method
    prefers])
  (:require [potemkin :as p]))

(require '[methodical macros impl interface util])

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
  dispatch-value
  matching-primary-methods
  matching-aux-methods
  default-dispatch-value
  prefers
  prefer-method
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
  primary-method
  applicable-primary-method
  effective-primary-method
  aux-methods
  default-primary-method
  default-aux-methods
  default-effective-method
  dispatch-fn
  remove-all-primary-methods
  remove-all-aux-methods
  remove-all-aux-methods-for-dispatch-val
  add-aux-method-with-unique-key
  remove-aux-method-with-unique-key
  remove-all-methods
  prefers?
  ;; destructive ops
  add-primary-method!
  remove-primary-method!
  remove-all-primary-methods!
  add-aux-method!
  remove-aux-method!
  remove-all-aux-methods!
  remove-all-aux-methods-for-dispatch-val!
  add-aux-method-with-unique-key!
  remove-aux-method-with-unique-key!
  remove-all-methods!
  prefer-method!])
