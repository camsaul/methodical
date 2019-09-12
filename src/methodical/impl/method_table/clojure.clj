(ns methodical.impl.method-table.clojure
  (:require [potemkin.types :as p.types]
            [pretty.core :refer [PrettyPrintable]])
  (:import methodical.interface.MethodTable))

(p.types/deftype+ ClojureMethodTable [m]
  PrettyPrintable
  (pretty [_]
    (if (seq m)
      (list 'clojure-method-table (count m) 'primary)
      '(clojure-method-table)))

  Object
  (equals [_ another]
    (and (instance? ClojureMethodTable another)
         (= m (.m ^ClojureMethodTable another))))

  MethodTable
  (primaryMethods [_]
    m)

  (auxMethods [_]
    nil)

  (addPrimaryMethod [this dispatch-val method]
    (let [new-m (assoc m dispatch-val method)]
      (if (= m new-m)
        this
        (ClojureMethodTable. new-m))))

  (removePrimaryMethod [this dispatch-val]
    (let [new-m (dissoc m dispatch-val)]
      (if (= m new-m)
        this
        (ClojureMethodTable. new-m))))

  (addAuxMethod [_ _ _ _]
    (throw (UnsupportedOperationException. "Clojure-style multimethods do not support auxiliary methods.")))

  (removeAuxMethod [_ _ _ _]
    (throw (UnsupportedOperationException. "Clojure-style multimethods do not support auxiliary methods."))))
