(ns methodical.impl.standard
  (:require [methodical.interface :as i]
            [pretty.core :refer [PrettyPrintable]]))

(defn- ^:static effective-method [impl, dispatch-value]
  (or (i/effective-method impl dispatch-value)
      (throw (UnsupportedOperationException. (format "No matching method for dispatch value %s" dispatch-value)))))

(defn- ^:static invoke-multifn
  ([impl]
   ((effective-method impl (i/dispatch-value (i/dispatcher impl)))))

  ([impl a]
   ((effective-method impl (i/dispatch-value (i/dispatcher impl) a)) a))

  ([impl a b]
   ((effective-method impl (i/dispatch-value (i/dispatcher impl) a b)) a b))

  ([impl a b c]
   ((effective-method impl (i/dispatch-value (i/dispatcher impl) a b c)) a b c))

  ([impl a b c d]
   ((effective-method impl (i/dispatch-value (i/dispatcher impl) a b c d)) a b c d))

  ([impl a b c d & more]
   (apply (effective-method impl (i/dispatch-value (i/dispatcher impl) a b c d more)) a b c d more)))

(deftype StandardMultiFn [impl mta]
  PrettyPrintable
  (pretty [_]
    (list 'multifn impl))

  Object
  (equals [_ another]
    (and (instance? StandardMultiFn another)
         (= impl (.-impl another))))

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

  i/MethodCombination
  (allowed-qualifiers [_]
    (i/allowed-qualifiers (i/method-combination impl)))

  (combine-methods [_ primary-methods aux-methods]
    (i/combine-methods (i/method-combination impl) primary-methods aux-methods))

  (transform-fn-tail [_ qualifier fn-tail]
    (i/transform-fn-tail (i/method-combination impl) qualifier fn-tail))

  i/Dispatcher
  (dispatch-value [_]
    (.dispatch-value (i/dispatcher impl)))
  (dispatch-value [_ a]
    (.dispatch-value (i/dispatcher impl) a))
  (dispatch-value [_ a b]
    (.dispatch-value (i/dispatcher impl) a b))
  (dispatch-value [_ a b c]
    (.dispatch-value (i/dispatcher impl) a b c))
  (dispatch-value [_ a b c d]
    (.dispatch-value (i/dispatcher impl) a b c d))
  (dispatch-value [_ a b c d more]
    (.dispatch-value (i/dispatcher impl) a b c d more))

  (matching-primary-methods [_ method-table dispatch-value]
    (i/matching-primary-methods (i/dispatcher impl) method-table dispatch-value))

  (matching-aux-methods [_ method-table dispatch-value]
    (i/matching-aux-methods (i/dispatcher impl) method-table dispatch-value))

  (default-dispatch-value [_]
    (i/default-dispatch-value (i/dispatcher impl)))

  (prefers [_]
    (i/prefers (i/dispatcher impl)))

  (prefer-method [this dispatch-val-x dispatch-val-y]
    (i/with-dispatcher this (i/prefer-method (i/dispatcher impl) dispatch-val-x dispatch-val-y)))

  i/MethodTable
  (primary-methods [_]
    (i/primary-methods (i/method-table impl)))

  (aux-methods [_]
    (i/aux-methods (i/method-table impl)))

  (add-primary-method [this dispatch-val method]
    (i/with-method-table this (i/add-primary-method (i/method-table impl) dispatch-val method)))

  (remove-primary-method [this dispatch-val]
    (i/with-method-table this (i/remove-primary-method (i/method-table impl) dispatch-val)))

  (add-aux-method [this qualifier dispatch-val method]
    (i/with-method-table this (i/add-aux-method (i/method-table impl) qualifier dispatch-val method)))

  (remove-aux-method [this qualifier dispatch-val method]
    (i/with-method-table this (i/remove-aux-method (i/method-table impl) qualifier dispatch-val method)))

  i/MultiFnImpl
  (method-combination [_]
    (i/method-combination impl))

  (dispatcher [_]
    (i/dispatcher impl))

  (with-dispatcher [this new-dispatcher]
    (assert (satisfies? i/Dispatcher new-dispatcher))
    (if (= (i/dispatcher impl) new-dispatcher)
      this
      (StandardMultiFn. (i/with-dispatcher impl new-dispatcher) mta)))

  (method-table [_]
    (i/method-table impl))

  (with-method-table [this new-method-table]
    (assert (satisfies? i/MethodTable new-method-table))
    (if (= (i/method-table impl) new-method-table)
      this
      (StandardMultiFn. (i/with-method-table impl new-method-table) mta)))

  (effective-method [_ dispatch-value]
    (i/effective-method impl dispatch-value))

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
