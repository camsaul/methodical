(ns methodical.impl.multifn.cached
  (:require [methodical.interface :as i]
            [pretty.core :refer [PrettyPrintable]])
  (:import [methodical.interface Cache MultiFnImpl]))

(deftype CachedMultiFnImpl [^MultiFnImpl impl, ^Cache cache]
  PrettyPrintable
  (pretty [_]
    (list 'cached-multifn-impl impl cache))

  Object
  (equals [_ another]
    (and (instance? CachedMultiFnImpl another)
         (= impl  (.impl ^CachedMultiFnImpl another))
         ;; TODO - does this make sense?
         (= (class cache) (class (.cache ^CachedMultiFnImpl another)))))

  MultiFnImpl
  (methodCombination [_]
    (i/method-combination impl))

  (dispatcher [_]
    (.dispatcher impl))

  (withDispatcher [this new-dispatcher]
    (let [new-impl (i/with-dispatcher impl new-dispatcher)]
      (if (= impl new-impl)
        this
        (CachedMultiFnImpl. new-impl (i/empty-copy cache)))))

  (methodTable [_]
    (i/method-table impl))

  (withMethodTable [this new-method-table]
    (let [new-impl (i/with-method-table impl new-method-table)]
      (if (= impl new-impl)
        this
        (CachedMultiFnImpl. new-impl (i/empty-copy cache)))))

  (effectiveMethod [_ dispatch-value]
    (or
     (.cachedMethod cache dispatch-value)
     (let [method (i/effective-method impl dispatch-value)]
       (i/cache-method! cache dispatch-value method)
       method))))
