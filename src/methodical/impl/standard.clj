(ns methodical.impl.standard
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [clojure.datafy :as datafy]
   [methodical.interface :as i]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (clojure.lang Named)
   (methodical.interface Dispatcher MethodCombination MethodTable MultiFnImpl)))

(set! *warn-on-reflection* true)

(defn- maybe-name [^MultiFnImpl impl]
  (if-let [nm (and (instance? Named impl) (name impl))]
    (str " " nm)
    ""))

(defn- handle-effective-method-exception [^Exception e mta]
  (if-let [dispatch-val (::unmatched-dispatch-value (ex-data e))]
    (throw (UnsupportedOperationException.
             (format "No matching%s method for dispatch value %s" (if-let [nm (:name mta)]
                                                                    (str " " nm)
                                                                    "")
                                                                  (pr-str dispatch-val))))
    ;; this wasn't an :unmatched-dispatch-value situation; just rethrow it
    (throw e)))

(defn- ^:static effective-method [^MultiFnImpl impl dispatch-value]
  (or (.effective-method impl dispatch-value)
      (throw
       (ex-info (format "No matching%s method for dispatch value %s" (maybe-name impl) (pr-str dispatch-value))
                {::unmatched-dispatch-value dispatch-value}))))

(defmacro ^:private invoke-multi
  "Utility macro for finding the effective method of `impl`, given the `args`, then catching an Exception on invoking
  the `effective-method`, where we look for the special case of `::unmatched-dispatch-value`. If we find that, we
  rethrow a regular `UnsupportedOperationException` including the method name and `pr-str` of the  unmatched dispatch
  value. If not, we simply rethrow the exception since it's not ours to handle."
  [impl mta & args]
  `(try
     (let [em# (effective-method ~impl (.dispatch-value ^Dispatcher (.dispatcher ~impl) ~@args))]
       (em# ~@args))
     (catch Exception e#
       (handle-effective-method-exception e# ~mta))))

(defn- ^:static invoke-multifn
  ([^MultiFnImpl impl mta]
   (invoke-multi impl mta))

  ([^MultiFnImpl impl mta a]
   (invoke-multi impl mta a))

  ([^MultiFnImpl impl mta a b]
   (invoke-multi impl mta a b))

  ([^MultiFnImpl impl mta a b c]
   (invoke-multi impl mta a b c))

  ([^MultiFnImpl impl mta a b c d]
   (invoke-multi impl mta a b c d))

  ([^MultiFnImpl impl mta a b c d & more]
   ;; TODO: possible to use the macro somehow in this case?
   (try (apply (effective-method impl (.dispatch-value ^Dispatcher (.dispatcher impl) a b c d more)) a b c d more)
        (catch Exception e
          (handle-effective-method-exception e mta)))))

(deftype StandardMultiFn [^MultiFnImpl impl mta]
  pretty/PrettyPrintable
  (pretty [_]
    (list 'multifn impl))

  Object
  (equals [_ another]
    (and (instance? StandardMultiFn another)
         (= impl (.impl ^StandardMultiFn another))))

  Named
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
  (allowed-qualifiers [_]
    (i/allowed-qualifiers (i/method-combination impl)))

  (combine-methods [_ primary-methods aux-methods]
    (i/combine-methods (i/method-combination impl) primary-methods aux-methods))

  (transform-fn-tail [_ qualifier fn-tail]
    (i/transform-fn-tail (i/method-combination impl) qualifier fn-tail))

  Dispatcher
  (dispatch-value [_]
    (.dispatch-value ^Dispatcher (.dispatcher impl)))
  (dispatch-value [_ a]
    (.dispatch-value ^Dispatcher (.dispatcher impl) a))
  (dispatch-value [_ a b]
    (.dispatch-value ^Dispatcher (.dispatcher impl) a b))
  (dispatch-value [_ a b c]
    (.dispatch-value ^Dispatcher (.dispatcher impl) a b c))
  (dispatch-value [_ a b c d]
    (.dispatch-value ^Dispatcher (.dispatcher impl) a b c d))
  (dispatch-value [_ a b c d more]
    (.dispatch-value ^Dispatcher (.dispatcher impl) a b c d more))

  (matching-primary-methods [_ method-table dispatch-value]
    (i/matching-primary-methods (.dispatcher impl) method-table dispatch-value))

  (matching-aux-methods [_ method-table dispatch-value]
    (i/matching-aux-methods (.dispatcher impl) method-table dispatch-value))

  (default-dispatch-value [_]
    (i/default-dispatch-value (.dispatcher impl)))

  (prefers [_]
    (i/prefers (.dispatcher impl)))

  (with-prefers [this new-prefers]
    (i/with-dispatcher this (i/with-prefers (.dispatcher impl) new-prefers)))

  (dominates? [_ dispatch-val-x dispatch-val-y]
    (i/dominates? (.dispatcher impl) dispatch-val-x dispatch-val-y))

  MethodTable
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

  MultiFnImpl
  (method-combination [_]
    (i/method-combination impl))

  (dispatcher [_]
    (.dispatcher impl))

  (with-dispatcher [this new-dispatcher]
    (assert (instance? Dispatcher new-dispatcher))
    (if (= (.dispatcher impl) new-dispatcher)
      this
      (StandardMultiFn. (i/with-dispatcher impl new-dispatcher) mta)))

  (method-table [_]
    (i/method-table impl))

  (with-method-table [this new-method-table]
    (assert (instance? MethodTable new-method-table))
    (if (= (i/method-table impl) new-method-table)
      this
      (StandardMultiFn. (i/with-method-table impl new-method-table) mta)))

  (effective-method [_this dispatch-value]
    (try (.effective-method impl dispatch-value)
         (catch Exception e
           (handle-effective-method-exception e mta))))

  java.util.concurrent.Callable
  (call [_]
    (invoke-multifn impl mta))

  java.lang.Runnable
  (run [_]
    (invoke-multifn impl mta))

  clojure.lang.IFn
  (invoke [_]
    (invoke-multifn impl mta))
  (invoke [_ a]
    (invoke-multifn impl mta  a))
  (invoke [_ a b]
    (invoke-multifn impl mta  a b))
  (invoke [_ a b c]
    (invoke-multifn impl mta  a b c))
  (invoke [_ a b c d]
    (invoke-multifn impl mta  a b c d))
  (invoke [_ a b c d e]
    (invoke-multifn impl mta  a b c d e))
  (invoke [_ a b c d e f]
    (invoke-multifn impl mta  a b c d e f))
  (invoke [_ a b c d e f g]
    (invoke-multifn impl mta  a b c d e f g))
  (invoke [_ a b c d e f g h]
    (invoke-multifn impl mta  a b c d e f g h))
  (invoke [_ a b c d e f g h i]
    (invoke-multifn impl mta  a b c d e f g h i))
  (invoke [_ a b c d e f g h i j]
    (invoke-multifn impl mta  a b c d e f g h i j))
  (invoke [_ a b c d e f g h i j k]
    (invoke-multifn impl mta  a b c d e f g h i j k))
  (invoke [_ a b c d e f g h i j k l]
    (invoke-multifn impl mta  a b c d e f g h i j k l))
  (invoke [_ a b c d e f g h i j k l m]
    (invoke-multifn impl mta  a b c d e f g h i j k l m))
  (invoke [_ a b c d e f g h i j k l m n]
    (invoke-multifn impl mta  a b c d e f g h i j k l m n))
  (invoke [_ a b c d e f g h i j k l m n o]
    (invoke-multifn impl mta  a b c d e f g h i j k l m n o))
  (invoke [_ a b c d e f g h i j k l m n o p]
    (invoke-multifn impl mta  a b c d e f g h i j k l m n o p))
  (invoke [_ a b c d e f g h i j k l m n o p q]
    (invoke-multifn impl mta  a b c d e f g h i j k l m n o p q))
  (invoke [_ a b c d e f g h i j k l m n o p q r]
    (invoke-multifn impl mta  a b c d e f g h i j k l m n o p q r))
  (invoke [_ a b c d e f g h i j k l m n o p q r s]
    (invoke-multifn impl mta  a b c d e f g h i j k l m n o p q r s))
  (invoke [_ a b c d e f g h i j k l m n o p q r s t]
    (invoke-multifn impl mta  a b c d e f g h i j k l m n o p q r s t))
  (invoke [_ a b c d e f g h i j k l m n o p q r s t args]
    (apply invoke-multifn impl mta a b c d e f g h i j k l m n o p q r s t args))
  (applyTo [_ args]
    (apply invoke-multifn impl mta args))

  clojure.protocols/Datafiable
  (datafy [this]
    (with-meta (merge (datafy/datafy impl)
                      (select-keys mta [:name :arglists :file :column :line])
                      (when (:ns mta)
                        {:ns (ns-name (:ns mta))})
                      (when (and (:ns mta) (:name mta))
                        {:name (symbol (str (ns-name (:ns mta))) (str (:name mta)))})
                      {:class (class this)})
      mta))

  describe/Describable
  (describe [_this]
    (let [{mf-name :name, mf-ns :ns, :keys [file line]} mta]
      (str (pr-str mf-name)
           (let [message (str
                          (when mf-ns
                            (format "[[%s]]" (ns-name mf-ns)))
                          (cond
                            (and file line) (format " (%s:%d)" file line)
                            file            (str \space file)
                            :else           ""))]
             (when (seq message)
               (format " is defined in %s." message)))
           \newline \newline
           (describe/describe impl)))))

(defn multifn?
  "True if `x` is an instance of `StandardMultiFn`."
  [x]
  (instance? StandardMultiFn x))
