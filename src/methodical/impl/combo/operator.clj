(ns methodical.impl.combo.operator
  "Method combinations strategies based on the non-default method combination types in CLOS. All non-default method
  combinations follow the same basic pattern:

  ```clj
  (operator (primary-method-1 args)
            (primary-method-2 args)
            (primary-method-3 args)))
  ```

  (Example from \"Object-Oriented Programming in Common Lisp\", Keene 1988.)

  The non-default method combinations each support primary methods and `:around` methods, but not `:before` or
  `:after`. Unlike the standard combination, primary methods do not support `call-next-method` (`next-method` in
  Methodical).

  There are 9 built-in method combinations types in CLOS, excluding `standard`: `progn`, `append`, `list`, `nconc`,
  `and`, `or`, `max`, `min`, and `+`. These are mostly the same in the implementation below, with the following
  exceptions:

  * The `progn` combo is instead named `do`, which you probably could have guessed.

  * `list` has been replaced by `seq`, which returns a lazy sequence -- a very Clojurey improvement.

  * Both `nconc` and `append` concatenate lists, but `nconc` does it destructively; `append` copies all arguments
    except the last. The Clojure equivalent of either is `concat` which is what I have named the method combination
    below. We actually do one better than CLOS and return a lazy sequence, but `lazy-cat` seemed like a cumbersome name
    for the combo.

  One last difference: unlike CLOS operator method combinations, primary method implementations *are not* qualified by
  their operator.

  ```clj
  ;; CLOS
  (defmethod total-electric-supply + ((city city))
    ...)

  ;; Methodical
  (defmethod total-electric-supply :city
    [city]
    ...)
  ```"
  (:refer-clojure :exclude [methods])
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [clojure.core.specs.alpha]
   [clojure.spec.alpha :as s]
   [methodical.impl.combo.common :as combo.common]
   [methodical.interface]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (methodical.interface MethodCombination)))

(set! *warn-on-reflection* true)

(comment clojure.core.specs.alpha/keep-me ; for the specs
         methodical.interface/keep-me)

(defmulti operator
  "Define a new operator that can be used as part of an `OperatorMethodCombination`. See examples below for more
  details. Prefer using the `defoperator` macro to adding a method to this directly."
  {:arglists '([operator-name])}
  keyword)

(defn- invoke-fn
  ([]               (fn [method] (method)))
  ([a]              (fn [method] (method a)))
  ([a b]            (fn [method] (method a b)))
  ([a b c]          (fn [method] (method a b c)))
  ([a b c d]        (fn [method] (method a b c d)))
  ([a b c d & more] (fn [method] (apply method a b c d more))))

(defn- reducing-operator
  [reducer]
  (comp reducer invoke-fn))

(defn ^:no-doc combine-methods-with-operator
  "Part of the impl for [[defoperator]]."
  [f]
  (fn combine*
    ([primary-methods]
     (when (seq primary-methods)
       (reducing-operator (f primary-methods))))

    ([primary-methods {:keys [around]}]
     (combo.common/apply-around-methods (combine* primary-methods) around))))

(defmacro defoperator
  "Define a new operator that can be used as part of an `OperatorMethodCombination`. See examples below for more
  details."
  [operator-name [methods-binding invoke-binding] & body]
  `(let [fn# (combine-methods-with-operator
              (fn [~methods-binding]
                (fn [~invoke-binding]
                  ~@body)))]
     (defmethod operator ~(keyword operator-name)
       [~'_]
       fn#)))

(s/fdef defoperator
  :args (s/cat :operator-name keyword?
               :bindings      (s/spec (s/cat :methods :clojure.core.specs.alpha/binding-form
                                             :invoke  symbol?))
               :body          (s/+ any?))
  :ret any?)

;;;; ### Predefined operators

(defoperator :do [methods invoke]
  (loop [[method & more] methods]
    (let [result (invoke method)]
      (if (seq more)
        (recur more)
        result))))

(defoperator :seq [methods invoke]
  ((fn seq* [[method & more]]
     (lazy-seq
      (cons
       (invoke method)
       (when (seq more)
         (seq* more)))))
   methods))

(defoperator :concat [methods invoke]
  ((fn seq* [[method & more]]
     (lazy-seq
      (concat
       (invoke method)
       (when (seq more)
         (seq* more)))))
   methods))

(defoperator :and [methods invoke]
  (loop [[method & more] methods]
    (let [result (invoke method)]
      (if (and result (seq more))
        (recur more)
        result))))

(defoperator :or [methods invoke]
  (loop [[method & more] methods]
    (or (invoke method)
        (when (seq more)
          (recur more)))))

(defoperator :max [methods invoke]
  (loop [current-max nil, [method & more] methods]
    (let [result  (invoke method)
          new-max (if current-max
                    (max result current-max)
                    result)]
      (if (seq more)
        (recur new-max more)
        new-max))))

(defoperator :min [methods invoke]
  (loop [current-min nil, [method & more] methods]
    (let [result  (invoke method)
          new-min (if current-min
                    (min result current-min)
                    result)]
      (if (seq more)
        (recur new-min more)
        new-min))))

(defoperator :+ [methods invoke]
  (loop [sum 0, [method & more] methods]
    (let [sum (+ (invoke method) sum)]
      (if (seq more)
        (recur sum more)
        sum))))


;;;; ### `OperatorMethodCombination`

(deftype OperatorMethodCombination [operator-name]
  pretty/PrettyPrintable
  (pretty [_]
    (list 'operator-method-combination operator-name))

  Object
  (equals [_ another]
    (and (instance? OperatorMethodCombination another)
         (= operator-name (.operator-name ^OperatorMethodCombination another))))

  MethodCombination
  (allowed-qualifiers [_]
    #{nil :around})

  (combine-methods [_ primary-methods {:keys [around]}]
    (when (seq primary-methods)
      (combo.common/apply-around-methods ((operator operator-name) primary-methods)
                                         around)))

  (transform-fn-tail [_ qualifier fn-tail]
    (if (= qualifier :around)
      (combo.common/add-implicit-next-method-args qualifier fn-tail)
      fn-tail))

  clojure.protocols/Datafiable
  (datafy [this]
    {:class    (class this)
     :operator operator-name})

  describe/Describable
  (describe [this]
    (format "It uses the method combination `%s`\nwith the operator `%s`."
            (.getCanonicalName (class this))
            (pr-str operator-name))))

(defn operator-method-combination
  "Create a new method combination using the operator named by `operator-name`, a keyword name of one of the
  `defoperator`: forms above or defined externallly.

    (operator-method-combination :max)"
  [operator-name]
  (assert (operator operator-name)
          (format "Invalid operator method combination: %s" operator-name))
  (OperatorMethodCombination. (keyword operator-name)))
