(ns methodical.impl.combo.clos
  "Method combination strategy that mimics the standard method combination in the Common Lisp Object System (CLOS).
  Supports `:before`, `:after`, and `:around` auxiliary methods. The values returned by `:before` and `:after` methods
  are ignored. Primary methods and around methods get an implicit `next-method` arg (see Methodical dox for more on
  what this means)."
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [methodical.impl.combo.common :as combo.common]
   [methodical.interface]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (methodical.interface MethodCombination)))

(set! *warn-on-reflection* true)

(comment methodical.interface/keep-me)

;; TODO - I'm 90% sure we can leverage the `reducing-operator` stuff in `combo.operator` to implement this
(defn- apply-befores [combined-method befores]
  (if (empty? befores)
    combined-method
    (fn
      ([]
       (doseq [f befores]
         (f))
       (combined-method))

      ([a]
       (doseq [f befores]
         (f a))
       (combined-method a))

      ([a b]
       (doseq [f befores]
         (f a b))
       (combined-method a b))

      ([a b c]
       (doseq [f befores]
         (f a b c))
       (combined-method a b c))

      ([a b c d]
       (doseq [f befores]
         (f a b c d))
       (combined-method a b c d))

      ([a b c d & more]
       (doseq [f befores]
         (apply f a b c d more))
       (apply combined-method a b c d more)))))

(defn- apply-afters [combined-method afters]
  (if (empty? afters)
    combined-method
    (let [afters          (reverse afters)
          apply-after-fns (fn [result]
                            (doseq [f afters]
                              (f result))
                            result)]
      (comp apply-after-fns combined-method))))

(deftype CLOSStandardMethodCombination []
  pretty/PrettyPrintable
  (pretty [_]
    '(clos-method-combination))

  Object
  (equals [_ another]
    (instance? CLOSStandardMethodCombination another))

  MethodCombination
  (allowed-qualifiers [_]
    #{nil :before :after :around})

  (combine-methods [_ primary-methods {:keys [before after around]}]
    (some-> (combo.common/combine-primary-methods primary-methods)
            (apply-befores before)
            (apply-afters after)
            (combo.common/apply-around-methods around)))

  (transform-fn-tail [_ qualifier fn-tail]
    (combo.common/add-implicit-next-method-args qualifier fn-tail))

  clojure.protocols/Datafiable
  (datafy [this]
    {:class (class this)})

  describe/Describable
  (describe [this]
    (format "It uses the method combination `%s`." (.getCanonicalName (class this)))))
