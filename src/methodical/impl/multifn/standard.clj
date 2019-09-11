(ns methodical.impl.multifn.standard
  "Standard Methodical MultiFn impl, which "
  (:require [methodical.interface :as i]
            [pretty.core :refer [PrettyPrintable]]))

(defn standard-effective-method
  "Build an effective method using the 'standard' technique, taking the dispatch-value-method pairs in the
  `method-table`, finiding applicable ones using `dispatcher`, and combining them using `method-combination`."
  [method-combination dispatcher method-table dispatch-value]
  (let [primary-methods (i/matching-primary-methods dispatcher method-table dispatch-value)
        aux-methods     (i/matching-aux-methods dispatcher method-table dispatch-value)]
    (i/combine-methods method-combination primary-methods aux-methods)))

(deftype StandardMultiFnImpl [combo dispatcher method-table]
  PrettyPrintable
  (pretty [_]
    (list 'standard-multifn-impl combo dispatcher method-table))

  Object
  (equals [_ another]
    (and (instance? StandardMultiFnImpl another)
         (and (= combo (.combo another))
              (= dispatcher (.dispatcher another))
              (= method-table (.method-table another)))))

  i/MultiFnImpl
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
