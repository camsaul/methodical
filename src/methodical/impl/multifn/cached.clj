(ns methodical.impl.multifn.cached
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [clojure.datafy :as datafy]
   [methodical.interface :as i]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (clojure.lang Named)
   (methodical.interface Cache MultiFnImpl)))

(set! *warn-on-reflection* true)

(deftype CachedMultiFnImpl [^MultiFnImpl impl ^Cache cache]
  pretty/PrettyPrintable
  (pretty [_]
    (list 'cached-multifn-impl impl cache))

  Object
  (equals [_ another]
    (and (instance? CachedMultiFnImpl another)
         (= impl  (.impl ^CachedMultiFnImpl another))
         ;; TODO - does this make sense?
         (= (class cache) (class (.cache ^CachedMultiFnImpl another)))))

  Named
  (getName [_]
    (when (instance? Named impl)
      (name impl)))
  (getNamespace [_]
    (when (instance? Named impl)
      (namespace impl)))

  MultiFnImpl
  (method-combination [_]
    (i/method-combination impl))

  (dispatcher [_]
    (.dispatcher impl))

  (with-dispatcher [this new-dispatcher]
    (let [new-impl (i/with-dispatcher impl new-dispatcher)]
      (if (= impl new-impl)
        this
        (CachedMultiFnImpl. new-impl (i/empty-copy cache)))))

  (method-table [_]
    (i/method-table impl))

  (with-method-table [this new-method-table]
    (let [new-impl (i/with-method-table impl new-method-table)]
      (if (= impl new-impl)
        this
        (CachedMultiFnImpl. new-impl (i/empty-copy cache)))))

  (effective-method [_ dispatch-value]
    (or
     (.cached-method cache dispatch-value)
     ;; just like vanilla multimethods, we will add a new entry for every unique dispatch value we encounter, so
     ;; there's an implicit assumption that dispatch values are bounded
     ;;
     ;; build the effective method for dispatch value. We may end up throwing this out, but we currently need to build
     ;; it to determine the effective dispatch value.
     (let [method                     (i/effective-method impl dispatch-value)
           effective-dispatch-value   (:dispatch-value (meta method))
           ;; If a method with the same effective dispatch value is already cached, add the existing method to the
           ;; cache for dispatch value. This way we don't end up with a bunch of duplicate methods impls for various
           ;; dispatch values that have the same effective dispatch value
           cached-effective-dv-method (.cached-method cache effective-dispatch-value)
           method                     (or cached-effective-dv-method method)]
       ;; Make sure the method was cached for the effective dispatch value as well, that way if some less-specific
       ;; dispatch value comes along with the same effective dispatch value we can use the existing method
       (when-not cached-effective-dv-method
         (i/cache-method! cache effective-dispatch-value method))
       (i/cache-method! cache dispatch-value method)
       method)))

  clojure.protocols/Datafiable
  (datafy [this]
    (assoc (datafy/datafy impl)
           :class (class this)
           :cache (datafy/datafy cache)))

  describe/Describable
  (describe [_this]
    (str (describe/describe cache)
         \newline \newline
         (describe/describe impl))))
