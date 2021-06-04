(ns methodical.impl.multifn.standard
  "Standard Methodical MultiFn impl, which "
  (:require [methodical.impl.dispatcher.common :as dispatcher.common]
            [methodical.interface :as i]
            [potemkin.types :as p.types]
            [pretty.core :refer [PrettyPrintable]])
  (:import [methodical.interface Dispatcher MethodCombination MethodTable MultiFnImpl]))

(defn- effective-dispatch-value
  [dispatcher [most-specific-primary-method] aux-methods]
  (let [most-specific-aux-methods (map first (vals aux-methods))
        dispatch-values           (->> (cons most-specific-primary-method most-specific-aux-methods)
                                       (map meta)
                                       (map :dispatch-value)
                                       (filter some?))]
    (first
     (sort-by
      identity
      (dispatcher.common/domination-comparitor (partial i/dominates? dispatcher))
      dispatch-values))))

(defn standard-effective-method
  "Build an effective method using the 'standard' technique, taking the dispatch-value-method pairs in the
  `method-table`, finiding applicable ones using `dispatcher`, and combining them using `method-combination`."
  [method-combination dispatcher method-table dispatch-value]
  (let [primary-methods (i/matching-primary-methods dispatcher method-table dispatch-value)
        aux-methods     (i/matching-aux-methods dispatcher method-table dispatch-value)]
    (some-> (i/combine-methods method-combination primary-methods aux-methods)
            (with-meta {:dispatch-value (effective-dispatch-value dispatcher primary-methods aux-methods)}))))

(p.types/deftype+ StandardMultiFnImpl [^MethodCombination combo
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
  (method-combination [_]
    combo)

  (dispatcher [_]
    dispatcher)

  (with-dispatcher [this new-dispatcher]
    (if (= dispatcher new-dispatcher)
      this
      (StandardMultiFnImpl. combo new-dispatcher method-table)))

  (method-table [_]
    method-table)

  (with-method-table [this new-method-table]
    (if (= method-table new-method-table)
      this
      (StandardMultiFnImpl. combo dispatcher new-method-table)))

  (effective-method [_ dispatch-value]
    (standard-effective-method combo dispatcher method-table dispatch-value)))
