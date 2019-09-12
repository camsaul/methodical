(ns methodical.impl.dispatcher.standard
  "A single-hierarchy dispatcher that behaves similarly to the way multimethod dispatch is done by vanilla Clojure
  multimethods, but with added support for auxiliary methods."
  (:refer-clojure :exclude [prefers prefer-method])
  (:require [methodical.impl.dispatcher.common :as dispatcher.common]
            [methodical.interface :as i]
            [potemkin.types :as p.types]
            [pretty.core :refer [PrettyPrintable]])
  (:import methodical.interface.Dispatcher))

(defn- matching-primary-pairs-excluding-default
  "Return a sequence of pairs of `[dispatch-value method]` for all applicable dispatch values, excluding the default
  method (if any); pairs are sorted in order from most-specific to least-specific."
  [hierarchy prefs method-map dispatch-value]
  (let [matches        (for [[a-dispatch-val method] method-map
                             :when                   (isa? hierarchy dispatch-value a-dispatch-val)]
                         [a-dispatch-val method])]
    (when (seq matches)
      (sort-by first (dispatcher.common/domination-comparitor hierarchy prefs dispatch-value) matches))))

(defn- ambiguous-error-fn [dispatch-val this-dispatch-val next-dispatch-val]
  (fn [& _]
    (throw
     (IllegalArgumentException.
      (format "Multiple methods match dispatch value: %s -> %s and %s, and neither is preferred."
              dispatch-val this-dispatch-val next-dispatch-val)))))

(defn- unambiguous-pairs-seq
  "Given a sequence of `[dispatch-value primary-method]` pairs, return a sequence that replaces the method in each pair
  with one that will throw an Exception if the dispatch value in the *following* pair is equally specific."
  [hierarchy prefs dispatch-val [[this-dispatch-val this-method]
                                 [next-dispatch-val :as next-pair]
                                 & more-pairs :as pairs]]
  {:pre [(every? sequential? pairs)]}
  (when (seq pairs)
    (let [this-pair [this-dispatch-val
                     (if (and next-pair
                              (dispatcher.common/ambiguous? hierarchy prefs dispatch-val
                                                            this-dispatch-val next-dispatch-val))
                       (ambiguous-error-fn dispatch-val this-dispatch-val next-dispatch-val)
                       this-method)]]
      (cons this-pair (when next-pair
                        (unambiguous-pairs-seq hierarchy prefs dispatch-val (cons next-pair more-pairs)))))))

(defn matching-primary-methods
  "Return a lazy sequence of applicable primary methods for `dispatch-value`, sorted from most-specific to
  least-specific. Replaces methods whose dispatch value is ambiguously specific with the next matching method with
  ones that throw Exceptions when invoked."
  [hierarchy prefs default-value method-table dispatch-value]
  {:pre [(map? hierarchy)]}
  (let [pairs          (unambiguous-pairs-seq
                        hierarchy
                        prefs
                        dispatch-value
                        (matching-primary-pairs-excluding-default hierarchy
                                                                  prefs
                                                                  (i/primary-methods method-table)
                                                                  dispatch-value))
        default-method (when (not= dispatch-value default-value)
                         (get (i/primary-methods method-table) default-value))]
    (concat
     (map second pairs)
     (when (and default-method
                (not (contains? (set (map first pairs)) default-value)))
       [default-method]))))

(defn- matching-aux-pairs
  [hierarchy prefs default-value method-table qualifier dispatch-value]
  (let [pairs           (for [[dv methods] (get (i/aux-methods method-table) qualifier)
                              :when        (isa? hierarchy dispatch-value dv)
                              method       methods]
                          [dv method])
        sorted          (sort-by first (dispatcher.common/domination-comparitor hierarchy prefs dispatch-value) pairs)
        default-methods (when-not (contains? (set (map first pairs)) dispatch-value)
                          (get-in (i/aux-methods method-table) [qualifier default-value]))]
    (concat sorted (for [method default-methods]
                     [default-value method]))))

(defn matching-aux-methods
  "Return a map of aux method qualifier -> sequence of applicable methods for `dispatch-value`, sorted from
  most-specific to least-specific."
  [hierarchy prefs default-value method-table dispatch-value]
  {:pre [(map? hierarchy)]}
  (into {} (for [[qualifier] (i/aux-methods method-table)
                 :let        [pairs (matching-aux-pairs hierarchy prefs default-value
                                                        method-table qualifier dispatch-value)]
                 :when       (seq pairs)]
             [qualifier (map second pairs)])))


(p.types/deftype+ StandardDispatcher [dispatch-fn hierarchy-var default-value prefs]
  PrettyPrintable
  (pretty [_]
    (concat ['standard-dispatcher dispatch-fn]
            (when (not= hierarchy-var #'clojure.core/global-hierarchy)
              [:hierarchy hierarchy-var])
            (when (not= default-value :default)
              [:default-value default-value])
            (when (seq prefs)
              [:prefers prefs])))

  Object
  (equals [_ another]
    (and
     (instance? StandardDispatcher another)
     (let [^StandardDispatcher another another]
       (and
        (= dispatch-fn (.dispatch-fn another))
        (= hierarchy-var (.hierarchy-var another))
        (= default-value (.default-value another))
        (= prefs (.prefs another))))))

  Dispatcher
  (dispatchValue [_]              (dispatch-fn))
  (dispatchValue [_ a]            (dispatch-fn a))
  (dispatchValue [_ a b]          (dispatch-fn a b))
  (dispatchValue [_ a b c]        (dispatch-fn a b c))
  (dispatchValue [_ a b c d]      (dispatch-fn a b c d))
  (dispatchValue [_ a b c d more] (apply dispatch-fn a b c d more))

  (matchingPrimaryMethods [_ method-table dispatch-value]
    (matching-primary-methods (var-get hierarchy-var) prefs default-value method-table dispatch-value))

  (matchingAuxMethods [_ method-table dispatch-value]
    (matching-aux-methods (var-get hierarchy-var) prefs default-value method-table dispatch-value))

  (defaultDispatchValue [_]
    default-value)

  (prefers [_]
    prefs)

  (preferMethod [this x y]
    (let [new-prefs (dispatcher.common/add-preference (partial isa? (var-get hierarchy-var)) prefs x y)]
      (if (= prefs new-prefs)
        this
        (StandardDispatcher. dispatch-fn hierarchy-var default-value new-prefs)))))
