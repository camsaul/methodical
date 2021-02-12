(ns methodical.impl.combo.clojure
  "Simple method combination strategy that mimics the way vanilla Clojure multimethods combine methods; that is, to say,
  not at all. Like vanilla Clojure multimethods, this method combination only supports primary methods."
  (:require methodical.interface
            [potemkin.types :as p.types]
            [pretty.core :refer [PrettyPrintable]])
  (:import methodical.interface.MethodCombination))

(p.types/deftype+ ClojureMethodCombination []
  PrettyPrintable
  (pretty [_]
    '(clojure-method-combination))

  Object
  (equals [_ another]
    (instance? ClojureMethodCombination another))

  MethodCombination
  (allowed-qualifiers [_]
    #{nil}) ; only primary methods

  (combine-methods [_ [primary-method] aux-methods]
    (when (seq aux-methods)
      (throw (UnsupportedOperationException. "Clojure-style multimethods do not support auxiliary methods.")))
    primary-method)

  (transform-fn-tail [_ _ fn-tail]
    fn-tail))
