(ns methodical.impl.combo.clojure-test
  (:require [clojure.test :as t]
            [methodical.core :as m]
            [methodical.impl.combo.clojure :as combo.clojure]
            [methodical.interface :as i]))

(t/deftest aux-methods-test
  (t/is (thrown-with-msg?
         UnsupportedOperationException
         #"Clojure-style multimethods do not support auxiliary methods."
         (i/combine-methods
          (combo.clojure/->ClojureMethodCombination)
          [(constantly :a)] {:before [(constantly :b)]}))
        "Clojure method combinations should thrown an Exception if you try to combine aux methods with them."))

(m/defmulti ^:private clojure-multifn
  class
  :combo (m/clojure-method-combination))

(m/defmethod clojure-multifn Object
  [_]
  Object)

(m/defmethod clojure-multifn Number
  [_]
  Number)

(t/deftest e2e-test
  (t/is (= Number
           (clojure-multifn 100))))

(t/deftest effective-method-metadata-test
  (doseq [[default? clojure-multifn]      {false clojure-multifn
                                           true  (m/add-primary-method clojure-multifn :default (fn [_] :default))}
          [klass expected-dispatch-value] {nil     (when default? :default)
                                           Object  Object
                                           Number  Number
                                           Integer Number}]
    (t/testing (format "%s with default? %s" (pr-str klass) default?)
      (t/is (= (when expected-dispatch-value
                 expected-dispatch-value)
               (:dispatch-value (meta (m/effective-primary-method clojure-multifn klass)))))
      (t/is (= (when expected-dispatch-value
                 expected-dispatch-value)
               (:dispatch-value (meta (m/effective-method clojure-multifn klass))))))))
