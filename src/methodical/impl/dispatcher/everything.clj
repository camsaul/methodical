(ns methodical.impl.dispatcher.everything
  (:refer-clojure :exclude [methods])
  (:require [methodical.impl.dispatcher.common :as dispatcher.common]
            [methodical.interface :as i]
            [potemkin.types :as p.types]
            [pretty.core :as pretty])
  (:import methodical.interface.Dispatcher))

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
          comparitor      (dispatcher.common/domination-comparitor (var-get hierarchy-var) prefs)]
      (for [[dispatch-value method] (sort-by first comparitor primary-methods)]
        (vary-meta method assoc :dispatch-value dispatch-value))))

  (matching-aux-methods [_ method-table _]
    (let [aux-methods (i/aux-methods method-table)
          comparitor  (dispatcher.common/domination-comparitor (var-get hierarchy-var) prefs)]
      (into {} (for [[qualifier dispatch-value->methods] aux-methods]
                 [qualifier (for [[dispatch-value methods] (sort-by first comparitor dispatch-value->methods)
                                  method methods]
                              (vary-meta method assoc :dispatch-value dispatch-value))]))))

  (default-dispatch-value [_]
    nil)

  (prefers [_]
    prefs)

  (prefer-method [this x y]
    (let [new-prefs (dispatcher.common/add-preference (partial isa? (deref hierarchy-var)) prefs x y)]
      (if (= prefs new-prefs)
        this
        (EverythingDispatcher. hierarchy-var new-prefs))))

  (dominates? [_ x y]
    (dispatcher.common/dominates? (var-get hierarchy-var) prefs x y)))
