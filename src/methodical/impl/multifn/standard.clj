(ns methodical.impl.multifn.standard
  "Standard Methodical MultiFn impl, which "
  (:require [methodical.impl.dispatcher.common :as dispatcher.common]
            [methodical.interface :as i]
            [potemkin.types :as p.types]
            [pretty.core :refer [PrettyPrintable]])
  (:import [methodical.interface Dispatcher MethodCombination MethodTable MultiFnImpl]))

;; "composite dispatch value" below just means a dispatch value consisting of multiple parts e.g. `[:x :y]` as opposed
;; to a single value like `:x`.

(defn sort-dispatch-values
  "Sort dispatch values in order from most-specific-overall to least-specific-overall."
  [dispatcher dispatch-values]
  (sort-by
   identity
   (dispatcher.common/domination-comparitor (partial i/dominates? dispatcher))
   dispatch-values))

(defn composite-effective-dispatch-value
  "Combine multiple composite dispatch values into a single composite dispatch value that has the overall most-specific
  arg for each position, e.g.

    ;; String is more specific than Object; ::parrot is more specific than ::bird
    (composite-effective-dispatch-value [[Object ::parrot] [String ::bird]]) ; -> [String ::parrot]

  If the most-specific dispatch value is not composite, it returns it directly."
  [dispatcher dispatch-values]
  ;; sort the values so in cases where there's ambiguity we take the keep the value in the overall-most-specific
  ;; dispatch value.
  (let [[most-specific-dispatch-value & more-dispatch-values] (sort-dispatch-values dispatcher dispatch-values)]
    ;; if the most-specific dispatch value is not composite, we can return it as-is -- there's no need to build a
    ;; composite dispatch value.
    (if-not (sequential? most-specific-dispatch-value)
      most-specific-dispatch-value
      ;; otherwise we need to combine stuff
      (reduce
       (fn [dv1 dv2]
         (map
          (fn [x y]
            (if (i/dominates? dispatcher y x)
              y
              x))
          dv1
          dv2))
       most-specific-dispatch-value
       (filter sequential? more-dispatch-values)))))

(defn effective-dispatch-value
  "Given matching `primary-methods` and `aux-methods` for `dispatch-value`, determine the effective dispatch value."
  {:arglists '([dispatcher primary-methods aux-methods])}
  [dispatcher [most-specific-primary-method] aux-methods]
  (let [dispatch-values (transduce
                         (comp cat (map meta) (map :dispatch-value) (filter some?))
                         conj
                         []
                         (cons [most-specific-primary-method] (vals aux-methods)))]
    (composite-effective-dispatch-value dispatcher dispatch-values)))

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
