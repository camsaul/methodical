(ns methodical.impl.combo.threaded
  (:refer-clojure :exclude [methods])
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [methodical.impl.combo.common :as combo.common]
   [methodical.interface]
   [methodical.util :as u]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (methodical.interface MethodCombination)))

(set! *warn-on-reflection* true)

(comment methodical.interface/keep-me)

(defn combine-methods-thread-first
  "Combine primary and auxiliary methods using a thread-first threading type."
  [primary-methods {:keys [before after around]}]
  (when-let [primary (combo.common/combine-primary-methods primary-methods)]
    (combo.common/apply-around-methods
     (if (and (empty? before) (empty? after))
       ;; If there is only the combined primary method, skip the wrapping dance and just return it.
       primary

       (let [methods       (concat before [primary] (reverse after))]
         (-> (reduce
              (fn [current nxt]
                (let [nxt (u/unwrap-fn-with-meta nxt)]
                  (fn combined-method-thread-first
                    ([]                     (current) (nxt))
                    ([a]                    (nxt (current a)))
                    ([a b]                  (nxt (current a b) b))
                    ([a b c]                (nxt (current a b c) b c))
                    ([a b c d]              (nxt (current a b c d) b c d))
                    ([a b c d e]            (nxt (current a b c d e) b c d e))
                    ([a b c d e f]          (nxt (current a b c d e f) b c d e f))
                    ([a b c d e f g]        (nxt (current a b c d e f g) b c d e f g))
                    ([a b c d e f g & more] (apply nxt (apply current a b c d e f g more) b c d e f g more)))))
              (u/unwrap-fn-with-meta (first methods))
              (rest methods))
             (u/fn-vary-meta assoc :methodical/combined-method? true))))
     around)))

(defn combine-methods-thread-last
  "Combine primary and auxiliary methods using a thread-last threading type."
  [primary-methods {:keys [before after around]}]
  (when-let [primary (combo.common/combine-primary-methods primary-methods)]
    (combo.common/apply-around-methods
     (if (and (empty? before) (empty? after))
       ;; If there is only the combined primary method, skip the wrapping dance and just return it.
       primary

       (let [methods (concat before [primary] (reverse after))]
         (-> (reduce
              (fn [current nxt]
                (let [nxt (u/unwrap-fn-with-meta nxt)]
                  (fn combined-method-thread-last
                    ([]                     (current) (nxt))
                    ([a]                    (nxt (current a)))
                    ([a b]                  (nxt a (current a b)))
                    ([a b c]                (nxt a b (current a b c)))
                    ([a b c d]              (nxt a b c (current a b c d)))
                    ([a b c d e]            (nxt a b c d (current a b c d e)))
                    ([a b c d e f]          (nxt a b c d e (current a b c d e f)))
                    ([a b c d e f g]        (nxt a b c d e f (current a b c d e f g)))
                    ([a b c d e f g & more] (apply nxt a b c d e f g (concat (butlast more) [(apply current a b c d e f g more)]))))))
              (u/unwrap-fn-with-meta (first methods))
              (rest methods))
             (u/fn-vary-meta assoc :methodical/combined-method? true))))
     around)))

(deftype ThreadingMethodCombination [threading-type]
  pretty/PrettyPrintable
  (pretty [_]
    (list 'threading-method-combination threading-type))

  MethodCombination
  Object
  (equals [_ another]
    (and (instance? ThreadingMethodCombination another)
         (= threading-type (.threading-type ^ThreadingMethodCombination another))))

  MethodCombination
  (allowed-qualifiers [_]
    #{nil :before :after :around})

  (combine-methods [_ primary-methods aux-methods]
    (case threading-type
      :thread-first (combine-methods-thread-first primary-methods aux-methods)
      :thread-last (combine-methods-thread-last primary-methods aux-methods)))

  (transform-fn-tail [_ qualifier fn-tail]
    (combo.common/add-implicit-next-method-args qualifier fn-tail))

  clojure.protocols/Datafiable
  (datafy [this]
    {:class          (class this)
     :threading-type threading-type})

  describe/Describable
  (describe [this]
    (format "It uses the method combination `%s`\nwith the threading strategy `%s`."
            (.getCanonicalName (class this))
            (pr-str threading-type))))

(defn threading-method-combination
  "Create a new `ThreadingMethodCombination` using the keyword `threading-type` strategy, e.g. `:thread-first` or
  `:thread-last`."
  [threading-type]
  {:pre [(#{:thread-first :thread-last} threading-type)]}
  (ThreadingMethodCombination. threading-type))
