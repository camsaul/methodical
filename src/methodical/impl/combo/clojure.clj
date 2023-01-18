(ns methodical.impl.combo.clojure
  "Simple method combination strategy that mimics the way vanilla Clojure multimethods combine methods; that is, to say,
  not at all. Like vanilla Clojure multimethods, this method combination only supports primary methods."
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [methodical.interface]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (methodical.interface MethodCombination)))

(set! *warn-on-reflection* true)

(comment methodical.interface/keep-me)

(deftype ClojureMethodCombination []
  pretty/PrettyPrintable
  (pretty [_]
    '(clojure-method-combination))

  Object
  (equals [_ another]
    (instance? ClojureMethodCombination another))

  MethodCombination
  (allowed-qualifiers [_]
    #{nil})                             ; only primary methods

  (combine-methods [_ [primary-method] aux-methods]
    (when (seq aux-methods)
      (throw (UnsupportedOperationException. "Clojure-style multimethods do not support auxiliary methods.")))
    primary-method)

  (transform-fn-tail [_this _qualifier fn-tail]
    fn-tail)

  clojure.protocols/Datafiable
  (datafy [this]
    {:class (class this)})

  describe/Describable
  (describe [this]
    (format "It uses the method combination `%s`." (.getCanonicalName (class this)))))
