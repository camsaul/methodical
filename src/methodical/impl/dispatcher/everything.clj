(ns methodical.impl.dispatcher.everything
  (:refer-clojure :exclude [methods])
  (:require [methodical.impl.dispatcher.common :as dispatcher.common]
            [methodical.interface :as i]
            [potemkin.types :as p.types]
            [pretty.core :as pretty])
  (:import methodical.interface.Dispatcher))

(set! *warn-on-reflection* true)

(p.types/deftype+ EverythingDispatcher [hierarchy-var prefs]
  pretty/PrettyPrintable
  (pretty [_]
    (cons
     'everything-dispatcher
     (concat
      (when (not= hierarchy-var #'clojure.core/global-hierarchy)
        [:hierarchy hierarchy-var])
      (when (seq prefs)
        [:prefers prefs]))))

  Object
  (equals [_ another]
    (and
     (instance? EverythingDispatcher another)
     (let [^EverythingDispatcher another another]
       (and
        (= hierarchy-var (.hierarchy-var another))
        (= prefs (.prefs another))))))

  Dispatcher
  (dispatch-value [_]                   nil)
  (dispatch-value [_ _a]                nil)
  (dispatch-value [_ _a _b]             nil)
  (dispatch-value [_ _a _b _c]          nil)
  (dispatch-value [_ _a _b _c _d]       nil)
  (dispatch-value [_ _a _b _c _d _more] nil)

  (matching-primary-methods [_ method-table _]
    (let [primary-methods (i/primary-methods method-table)
          comparatorr     (dispatcher.common/domination-comparator (deref hierarchy-var) prefs)]
      (for [[dispatch-value method] (sort-by first comparatorr primary-methods)]
        (vary-meta method assoc :dispatch-value dispatch-value))))

  (matching-aux-methods [_ method-table _]
    (let [aux-methods (i/aux-methods method-table)
          comparatorr (dispatcher.common/domination-comparator (deref hierarchy-var) prefs)]
      (into {} (for [[qualifier dispatch-value->methods] aux-methods]
                 [qualifier (for [[dispatch-value methods] (sort-by first comparatorr dispatch-value->methods)
                                  method methods]
                              (vary-meta method assoc :dispatch-value dispatch-value))]))))

  (default-dispatch-value [_]
    nil)

  (prefers [_]
    prefs)

  (with-prefers [_this new-prefs]
    (EverythingDispatcher. hierarchy-var new-prefs))

  (dominates? [_ x y]
    (dispatcher.common/dominates? (deref hierarchy-var) prefs x y)))
