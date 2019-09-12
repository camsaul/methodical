(ns methodical.impl.dispatcher.everything
  (:require [methodical.impl.dispatcher.common :as dispatcher.common]
            [methodical.interface :as i]
            [potemkin.types :as p.types]
            [pretty.core :refer [PrettyPrintable]])
  (:import methodical.interface.Dispatcher))

(p.types/deftype+ EverythingDispatcher [hierarchy-var prefs]
  PrettyPrintable
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
  (dispatchValue [_]              nil)
  (dispatchValue [_ a]            nil)
  (dispatchValue [_ a b]          nil)
  (dispatchValue [_ a b c]        nil)
  (dispatchValue [_ a b c d]      nil)
  (dispatchValue [_ a b c d more] nil)

  (matchingPrimaryMethods [_ method-table _]
    (let [primary-methods (i/primary-methods method-table)
          comparitor      (dispatcher.common/domination-comparitor (var-get hierarchy-var) prefs ::no-dispatch-value)]
      (map second (sort-by first comparitor primary-methods))))

  (matchingAuxMethods [_ method-table _]
    (let [aux-methods (i/aux-methods method-table)
          comparitor  (dispatcher.common/domination-comparitor (var-get hierarchy-var) prefs ::no-dispatch-value)]
      (into {} (for [[qualifier dispatch-value->methods] aux-methods]
                 [qualifier (mapcat second (sort-by first comparitor dispatch-value->methods))]))))

  (defaultDispatchValue [_]
    nil)

  (prefers [_]
    prefs)

  (preferMethod [this x y]
    (let [new-prefs (dispatcher.common/add-preference (partial isa? (var-get hierarchy-var)) prefs x y)]
      (if (= prefs new-prefs)
        this
        (EverythingDispatcher. hierarchy-var new-prefs)))))
