(ns methodical.impl.multifn.standard
  "Standard Methodical MultiFn impl, which "
  (:require [methodical.interface :as i]
            [pretty.core :refer [PrettyPrintable]])
  (:import [methodical.interface Dispatcher MethodCombination MethodTable MultiFnImpl]))

(defn standard-effective-method
  "Build an effective method using the 'standard' technique, taking the dispatch-value-method pairs in the
  `method-table`, finiding applicable ones using `dispatcher`, and combining them using `method-combination`."
  [method-combination dispatcher method-table dispatch-value]
  (let [primary-methods (i/matching-primary-methods dispatcher method-table dispatch-value)
        aux-methods     (i/matching-aux-methods dispatcher method-table dispatch-value)]
    (i/combine-methods method-combination primary-methods aux-methods)))

(deftype StandardMultiFnImpl [^MethodCombination combo
                              ^Dispatcher dispatcher
                              ^MethodTable method-table]
  PrettyPrintable
  (pretty [_]
    (list 'standard-multifn-impl combo dispatcher method-table))

  Object
  (equals [_ another]
    (and (instance? StandardMultiFnImpl another)
         (let [^StandardMultiFnImpl another another]
           (and (= combo (.combo another))
                (= dispatcher (.dispatcher another))
                (= method-table (.method-table another))))))

  MultiFnImpl
  (methodCombination [_]
    combo)

  (dispatcher [_]
    dispatcher)

  (withDispatcher [this new-dispatcher]
    (if (= dispatcher new-dispatcher)
      this
      (StandardMultiFnImpl. combo new-dispatcher method-table)))

  (methodTable [_]
    method-table)

  (withMethodTable [this new-method-table]
    (if (= method-table new-method-table)
      this
      (StandardMultiFnImpl. combo dispatcher new-method-table)))

  (effectiveMethod [_ dispatch-value]
    (standard-effective-method combo dispatcher method-table dispatch-value)))
