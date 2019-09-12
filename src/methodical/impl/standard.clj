(ns methodical.impl.standard
  (:require [methodical.interface :as i]
            [potemkin.types :as p.types]
            [pretty.core :refer [PrettyPrintable]])
  (:import [methodical.interface Dispatcher MethodCombination MethodTable MultiFnImpl]))

(defn- ^:static effective-method [^MultiFnImpl impl, dispatch-value]
  (or (.effectiveMethod impl dispatch-value)
      (throw (UnsupportedOperationException. (format "No matching method for dispatch value %s" dispatch-value)))))

(defn- ^:static invoke-multifn
  ([^MultiFnImpl impl]
   ((effective-method impl (.dispatchValue ^Dispatcher (.dispatcher impl)))))

  ([^MultiFnImpl impl a]
   ((effective-method impl (.dispatchValue ^Dispatcher (.dispatcher impl) a)) a))

  ([^MultiFnImpl impl a b]
   ((effective-method impl (.dispatchValue ^Dispatcher (.dispatcher impl) a b)) a b))

  ([^MultiFnImpl impl a b c]
   ((effective-method impl (.dispatchValue ^Dispatcher (.dispatcher impl) a b c)) a b c))

  ([^MultiFnImpl impl a b c d]
   ((effective-method impl (.dispatchValue ^Dispatcher (.dispatcher impl) a b c d)) a b c d))

  ([^MultiFnImpl impl a b c d & more]
   (apply (effective-method impl (.dispatchValue ^Dispatcher (.dispatcher impl) a b c d more)) a b c d more)))

(p.types/deftype+ StandardMultiFn [^MultiFnImpl impl mta]
  PrettyPrintable
  (pretty [_]
    (list 'multifn impl))

  Object
  (equals [_ another]
    (and (instance? StandardMultiFn another)
         (= impl (.impl ^StandardMultiFn another))))

  clojure.lang.Named
  (getName [_] (some-> (:name mta) name))
  (getNamespace [_] (some-> (:ns mta) ns-name name))

  clojure.lang.IObj
  (meta [_]
    mta)
  (withMeta [this new-meta]
    (if (= mta new-meta)
      this
      (StandardMultiFn. impl new-meta)))

  MethodCombination
  (allowedQualifiers [_]
    (i/allowed-qualifiers (i/method-combination impl)))

  (combineMethods [_ primary-methods aux-methods]
    (i/combine-methods (i/method-combination impl) primary-methods aux-methods))

  (transformFnTail [_ qualifier fn-tail]
    (i/transform-fn-tail (i/method-combination impl) qualifier fn-tail))

  Dispatcher
  (dispatchValue [_]
    (.dispatchValue ^Dispatcher (.dispatcher impl)))
  (dispatchValue [_ a]
    (.dispatchValue ^Dispatcher (.dispatcher impl) a))
  (dispatchValue [_ a b]
    (.dispatchValue ^Dispatcher (.dispatcher impl) a b))
  (dispatchValue [_ a b c]
    (.dispatchValue ^Dispatcher (.dispatcher impl) a b c))
  (dispatchValue [_ a b c d]
    (.dispatchValue ^Dispatcher (.dispatcher impl) a b c d))
  (dispatchValue [_ a b c d more]
    (.dispatchValue ^Dispatcher (.dispatcher impl) a b c d more))

  (matchingPrimaryMethods [_ method-table dispatch-value]
    (i/matching-primary-methods (.dispatcher impl) method-table dispatch-value))

  (matchingAuxMethods [_ method-table dispatch-value]
    (i/matching-aux-methods (.dispatcher impl) method-table dispatch-value))

  (defaultDispatchValue [_]
    (i/default-dispatch-value (.dispatcher impl)))

  (prefers [_]
    (i/prefers (.dispatcher impl)))

  (preferMethod [this dispatch-val-x dispatch-val-y]
    (i/with-dispatcher this (i/prefer-method (.dispatcher impl) dispatch-val-x dispatch-val-y)))

  MethodTable
  (primaryMethods [_]
    (i/primary-methods (i/method-table impl)))

  (auxMethods [_]
    (i/aux-methods (i/method-table impl)))

  (addPrimaryMethod [this dispatch-val method]
    (i/with-method-table this (i/add-primary-method (i/method-table impl) dispatch-val method)))

  (removePrimaryMethod [this dispatch-val]
    (i/with-method-table this (i/remove-primary-method (i/method-table impl) dispatch-val)))

  (addAuxMethod [this qualifier dispatch-val method]
    (i/with-method-table this (i/add-aux-method (i/method-table impl) qualifier dispatch-val method)))

  (removeAuxMethod [this qualifier dispatch-val method]
    (i/with-method-table this (i/remove-aux-method (i/method-table impl) qualifier dispatch-val method)))

  MultiFnImpl
  (methodCombination [_]
    (i/method-combination impl))

  (dispatcher [_]
    (.dispatcher impl))

  (withDispatcher [this new-dispatcher]
    (assert (instance? Dispatcher new-dispatcher))
    (if (= (.dispatcher impl) new-dispatcher)
      this
      (StandardMultiFn. (i/with-dispatcher impl new-dispatcher) mta)))

  (methodTable [_]
    (i/method-table impl))

  (withMethodTable [this new-method-table]
    (assert (instance? MethodTable new-method-table))
    (if (= (i/method-table impl) new-method-table)
      this
      (StandardMultiFn. (i/with-method-table impl new-method-table) mta)))

  (effectiveMethod [_ dispatch-value]
    (.effectiveMethod impl dispatch-value))

  java.util.concurrent.Callable
  (call [_]
    (invoke-multifn impl))

  java.lang.Runnable
  (run [_]
    (invoke-multifn impl))

  clojure.lang.IFn
  (invoke [_]
    (invoke-multifn impl))
  (invoke [_ a]
    (invoke-multifn impl a))
  (invoke [_ a b]
    (invoke-multifn impl a b))
  (invoke [_ a b c]
    (invoke-multifn impl a b c))
  (invoke [_ a b c d]
    (invoke-multifn impl a b c d))
  (invoke [_ a b c d e]
    (invoke-multifn impl a b c d e))
  (invoke [_ a b c d e f]
    (invoke-multifn impl a b c d e f))
  (invoke [_ a b c d e f g]
    (invoke-multifn impl a b c d e f g))
  (invoke [_ a b c d e f g h]
    (invoke-multifn impl a b c d e f g h))
  (invoke [_ a b c d e f g h i]
    (invoke-multifn impl a b c d e f g h i))
  (invoke [_ a b c d e f g h i j]
    (invoke-multifn impl a b c d e f g h i j))
  (invoke [_ a b c d e f g h i j k]
    (invoke-multifn impl a b c d e f g h i j k))
  (invoke [_ a b c d e f g h i j k l]
    (invoke-multifn impl a b c d e f g h i j k l))
  (invoke [_ a b c d e f g h i j k l m]
    (invoke-multifn impl a b c d e f g h i j k l m))
  (invoke [_ a b c d e f g h i j k l m n]
    (invoke-multifn impl a b c d e f g h i j k l m n))
  (invoke [_ a b c d e f g h i j k l m n o]
    (invoke-multifn impl a b c d e f g h i j k l m n o))
  (invoke [_ a b c d e f g h i j k l m n o p]
    (invoke-multifn impl a b c d e f g h i j k l m n o p))
  (invoke [_ a b c d e f g h i j k l m n o p q]
    (invoke-multifn impl a b c d e f g h i j k l m n o p q))
  (invoke [_ a b c d e f g h i j k l m n o p q r]
    (invoke-multifn impl a b c d e f g h i j k l m n o p q r))
  (invoke [_ a b c d e f g h i j k l m n o p q r s]
    (invoke-multifn impl a b c d e f g h i j k l m n o p q r s))
  (invoke [_ a b c d e f g h i j k l m n o p q r s t]
    (invoke-multifn impl a b c d e f g h i j k l m n o p q r s t))
  (invoke [_ a b c d e f g h i j k l m n o p q r s t args]
    (apply invoke-multifn impl a b c d e f g h i j k l m n o p q r s t args))
  (applyTo [_ args]
    (apply invoke-multifn impl args)))

(defn multifn?
  "True if `x` is an instance of `StandardMultiFn`."
  [x]
  (instance? StandardMultiFn x))
