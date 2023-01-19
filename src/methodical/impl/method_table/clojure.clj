(ns methodical.impl.method-table.clojure
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [methodical.impl.method-table.common :as method-table.common]
   [methodical.interface]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (methodical.interface MethodTable)))

(set! *warn-on-reflection* true)

(comment methodical.interface/keep-me)

(deftype ClojureMethodTable [m]
  pretty/PrettyPrintable
  (pretty [_]
    (if (seq m)
      (list 'clojure-method-table (count m) 'primary)
      '(clojure-method-table)))

  Object
  (equals [_ another]
    (and (instance? ClojureMethodTable another)
         (= m (.m ^ClojureMethodTable another))))

  MethodTable
  (primary-methods [_]
    m)

  (aux-methods [_]
    nil)

  (add-primary-method [this dispatch-val method]
    (let [new-m (assoc m dispatch-val method)]
      (if (= m new-m)
        this
        (ClojureMethodTable. new-m))))

  (remove-primary-method [this dispatch-val]
    (let [new-m (dissoc m dispatch-val)]
      (if (= m new-m)
        this
        (ClojureMethodTable. new-m))))

  (add-aux-method [_ _ _ _]
    (throw (UnsupportedOperationException. "Clojure-style multimethods do not support auxiliary methods.")))

  (remove-aux-method [_ _ _ _]
    (throw (UnsupportedOperationException. "Clojure-style multimethods do not support auxiliary methods.")))

  clojure.protocols/Datafiable
  (datafy [this]
    {:class   (class this)
     :primary (method-table.common/datafy-primary-methods m)})

  describe/Describable
  (describe [this]
    (format "It uses the method table `%s`. These primary methods are known:\n\n%s"
            (.getCanonicalName (class this))
            (method-table.common/describe-primary-methods m))))
