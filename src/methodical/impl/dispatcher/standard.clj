(ns methodical.impl.dispatcher.standard
  "A single-hierarchy dispatcher that behaves similarly to the way multimethod dispatch is done by vanilla Clojure
  multimethods, but with added support for auxiliary methods."
  (:refer-clojure :exclude [prefers methods])
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [methodical.impl.dispatcher.common :as dispatcher.common]
   [methodical.interface :as i]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (methodical.interface Dispatcher)))

(set! *warn-on-reflection* true)

(defn matching-primary-pairs-excluding-default
  "Return a sequence of pairs of `[dispatch-value method]` for all applicable dispatch values, excluding the default
  method (if any); pairs are sorted in order from most-specific to least-specific."
  [{:keys [hierarchy prefs method-map dispatch-value]}]
  {:pre [(map? method-map)]}
  (let [matches        (for [[a-dispatch-val method] method-map
                             :when                   (isa? hierarchy dispatch-value a-dispatch-val)]
                         [a-dispatch-val method])]
    (when (seq matches)
      (sort-by first (dispatcher.common/domination-comparator hierarchy prefs dispatch-value) matches))))

(defn- ambiguous-error-fn [dispatch-val method-1 dispatch-val-1 method-2 dispatch-val-2]
  (fn [& _]
    (let [multimethod-name (or (some-> (:multifn (meta method-1)) symbol)
                               (some-> (:multifn (meta method-2)) symbol)
                               ;; if we don't have the multimethod name for whatever reason then just use 'Multimethod'
                               ;; as a placeholder. The error message will still make sense.
                               "Multimethod")]
      (throw
       (ex-info (format "%s: multiple methods match dispatch value: %s -> %s and %s, and neither is preferred."
                        multimethod-name dispatch-val dispatch-val-1 dispatch-val-2)
                {:method-1 (assoc (select-keys (meta method-1) [:ns :file :line])
                                  :dispatch-value dispatch-val-1)
                 :method-2 (assoc (select-keys (meta method-2) [:ns :file :line])
                                  :dispatch-value dispatch-val-2)})))))

(defn unambiguous-pairs-seq
  "Given a sequence of `[dispatch-value primary-method]` pairs, return a sequence that replaces the method in each pair
  with one that will throw an Exception if the dispatch value in the *following* pair is equally specific."
  [{:keys [hierarchy prefs dispatch-value ambiguous-fn]
    :or   {ambiguous-fn dispatcher.common/ambiguous?}
    :as   opts}
   [[this-dispatch-val this-method :as _this-pair]
    [next-dispatch-val next-method :as next-pair]
    & more-pairs :as pairs]]
  {:pre [(every? sequential? pairs)]}
  (when (seq pairs)
    (let [this-pair [this-dispatch-val
                     (if (and next-pair
                              (ambiguous-fn hierarchy prefs dispatch-value this-dispatch-val next-dispatch-val))
                       (ambiguous-error-fn dispatch-value this-method this-dispatch-val next-method next-dispatch-val)
                       this-method)]]
      (cons this-pair (when next-pair
                        (unambiguous-pairs-seq opts (cons next-pair more-pairs)))))))

(defn matching-primary-methods
  "Return a lazy sequence of applicable primary methods for `dispatch-value`, sorted from most-specific to
  least-specific. Replaces methods whose dispatch value is ambiguously specific with the next matching method with
  ones that throw Exceptions when invoked."
  {:arglists '([{:keys [hierarchy prefs default-value method-table dispatch-value]}])}
  [{:keys [hierarchy default-value method-table dispatch-value], :as opts}]
  {:pre [(map? hierarchy) (some? method-table)]}
  (let [opts           (assoc opts :method-map (i/primary-methods method-table))
        pairs          (unambiguous-pairs-seq opts (matching-primary-pairs-excluding-default opts))
        default-method (when (not= dispatch-value default-value)
                         (get (i/primary-methods method-table) default-value))]
    (concat
     (for [[dispatch-value method] pairs]
       (vary-meta method assoc :dispatch-value dispatch-value))
     (when (and default-method
                (not (contains? (set (map first pairs)) default-value)))
       [(vary-meta default-method assoc :dispatch-value default-value)]))))

(defn- matching-aux-pairs-excluding-default
  "Return pairs of `[dispatch-value method]` of applicable aux methods, *excluding* default aux methods. Pairs are
  ordered from most-specific to least-specific."
  [qualifier {:keys [hierarchy prefs method-table dispatch-value]}]
  {:pre [(map? hierarchy)]}
  (let [pairs           (for [[dv methods] (get (i/aux-methods method-table) qualifier)
                              :when        (isa? hierarchy dispatch-value dv)
                              method       methods]
                          [dv method])]
    (sort-by first (dispatcher.common/domination-comparator hierarchy prefs dispatch-value) pairs)))

(defn matching-aux-pairs
  "Return pairs of `[dispatch-value method]` of applicable aux methods, *including* default aux methods. Pairs are
  ordered from most-specific to least-specific."
  [qualifier {:keys [default-value method-table dispatch-value], :as opts}]
  (let [pairs           (matching-aux-pairs-excluding-default qualifier opts)
        default-methods (when-not (contains? (set (map first pairs)) dispatch-value)
                          (get-in (i/aux-methods method-table) [qualifier default-value]))
        default-pairs   (for [method default-methods]
                          [default-value method])]
    (concat pairs default-pairs)))

(defn matching-aux-methods
  "Return a map of aux method qualifier -> sequence of applicable methods for `dispatch-value`, sorted from
  most-specific to least-specific."
  [{:keys [method-table] :as opts}]
  (into {} (for [[qualifier] (i/aux-methods method-table)
                 :let        [pairs (matching-aux-pairs qualifier opts)]
                 :when       (seq pairs)]
             [qualifier (for [[dispatch-value method] pairs]
                          (vary-meta method assoc :dispatch-value dispatch-value))])))

(deftype StandardDispatcher [dispatch-fn hierarchy-var default-value prefs]
  pretty/PrettyPrintable
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
        (= dispatch-fn   (.dispatch-fn another))
        (= hierarchy-var (.hierarchy-var another))
        (= default-value (.default-value another))
        (= prefs         (.prefs another))))))

  Dispatcher
  (dispatch-value [_]              (dispatch-fn))
  (dispatch-value [_ a]            (dispatch-fn a))
  (dispatch-value [_ a b]          (dispatch-fn a b))
  (dispatch-value [_ a b c]        (dispatch-fn a b c))
  (dispatch-value [_ a b c d]      (dispatch-fn a b c d))
  (dispatch-value [_ a b c d more] (apply dispatch-fn a b c d more))

  (matching-primary-methods [_ method-table dispatch-value]
    (matching-primary-methods
     {:hierarchy      (deref hierarchy-var)
      :prefs          prefs
      :default-value  default-value
      :method-table   method-table
      :dispatch-value dispatch-value}))

  (matching-aux-methods [_ method-table dispatch-value]
    (matching-aux-methods
     {:hierarchy      (deref hierarchy-var)
      :prefs          prefs
      :default-value  default-value
      :method-table   method-table
      :dispatch-value dispatch-value}))

  (default-dispatch-value [_]
    default-value)

  (prefers [_]
    prefs)

  (with-prefers [_this new-prefs]
    (StandardDispatcher. dispatch-fn hierarchy-var default-value new-prefs))

  (dominates? [_ x y]
    (dispatcher.common/dominates? (deref hierarchy-var) prefs default-value x y))

  clojure.protocols/Datafiable
  (datafy [this]
    {:class         (class this)
     :dispatch-fn   dispatch-fn
     :default-value default-value
     :hierarchy     hierarchy-var
     :prefs         prefs})

  describe/Describable
  (describe [this]
    (format "It uses the dispatcher `%s`\nwith hierarchy `%s`\nand prefs `%s`.\n\nThe default value is `%s`."
            (.getCanonicalName (class this))
            (pr-str hierarchy-var)
            (pr-str prefs)
            (pr-str default-value))))
