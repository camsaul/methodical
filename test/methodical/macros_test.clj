(ns methodical.macros-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :as t]
   [methodical.impl :as impl]
   [methodical.interface :as i]
   [methodical.macros :as macros]
   [methodical.util :as u]
   [potemkin.namespaces :as p.namespaces]))

;;; so the tests that do macroexansion work correctly regardless of namespace
(t/use-fixtures :each (fn [thunk]
                        (binding [*ns* (the-ns 'methodical.macros-test)]
                          (thunk))))

(t/deftest parse-defmulti-args-test
  (t/are [args parsed] (= (quote parsed)
                          (s/conform :methodical.macros/defmulti-args (quote args)))
    (clojure-multifn
     class
     :combo (macros/clojure-method-combination))
    {:name-symb   clojure-multifn
     :dispatch-fn class
     :options     [{:k :combo, :v (macros/clojure-method-combination)}]})

  (t/testing "Throw error on invalid args (#36)"
    (t/is (thrown?
           clojure.lang.Compiler$CompilerException
           (macroexpand
            '(macros/defmulti multifn :default
               [x y z]
               :ok))))))

(t/deftest method-fn-symbol-test
  (letfn [(method-fn-symbol [dispatch-value]
            (#'macros/method-fn-symbol 'my-multimethod "primary" dispatch-value))]
    (t/testing "Keyword dispatch value"
      (t/is (= 'my-multimethod-primary-method-something
               (method-fn-symbol :something))))
    (t/testing "Symbol dispatch value"
      (t/is (= 'my-multimethod-primary-method-String
               (method-fn-symbol 'String)))
      (t/testing "with namespace"
        (t/is (= 'my-multimethod-primary-method-somewhere-something-cool
                 (method-fn-symbol 'somewhere/something.cool)))))
    (t/testing "String dispatch value"
      (t/is (= 'my-multimethod-primary-method-Wow-OK
               (method-fn-symbol "Wow OK"))))
    (t/testing "Composite dispatch value"
      (t/is (= 'my-multimethod-primary-method-k1-String
               (method-fn-symbol [:k1 'String]))))
    (t/testing "Arbitrary expression dispatch value"
      (t/is (= 'my-multimethod-primary-method-if-true-String-Integer
               (method-fn-symbol '[(if true String Integer)]))))
    (t/testing "Munged function name should include keyword namespaces"
      (t/is (= 'my-multimethod-primary-method-somewhere-something
               (method-fn-symbol :somewhere/something))))
    (t/testing "Munged function name should replace periods in keywords (#60)"
      (t/testing "Munged function name should include keyword namespaces"
        (t/is (= 'my-multimethod-primary-method-somewhere-else-something
                 (method-fn-symbol :somewhere.else/something)))
        (t/is (= 'my-multimethod-primary-method-somewhere-something-else
                 (method-fn-symbol :somewhere/something-else)))))
    (t/testing "Illegal characters"
      (t/is (= 'my-multimethod-primary-method-can_SINGLEQUOTE_t-use-this
               (method-fn-symbol "can't use this"))))))

(macros/defmulti ^:private mf1 :type)

(t/deftest parse-defmethod-args-test
  (t/are [args parsed] (= (quote parsed)
                          (#'macros/parse-defmethod-args mf1 (quote args)))
    (:x [m] body1 body2)
    {:method-type    :primary
     :dispatch-value :x
     :fn-tail        ([m] body1 body2)}

    (:x "Docstring" [m] body1 body2)
    {:method-type    :primary
     :dispatch-value :x
     :docstring      "Docstring"
     :fn-tail        ([m] body1 body2)}

    (:after :x [m] body1 body2)
    {:method-type    :aux
     :qualifier      :after
     :dispatch-value :x
     :fn-tail        ([m] body1 body2)}

    (:after :x "Docstring" [m] body1 body2)
    {:method-type    :aux
     :qualifier      :after
     :dispatch-value :x
     :docstring      "Docstring"
     :fn-tail        ([m] body1 body2)}

    ("str-dv" [m] body1 body2)
    {:method-type    :primary
     :dispatch-value "str-dv"
     :fn-tail        ([m] body1 body2)}

    (:before "str-dv" [m] body1 body2)
    {:method-type    :aux
     :qualifier      :before
     :dispatch-value "str-dv"
     :fn-tail        ([m] body1 body2)}

    (:x ([x] y z) ([x y] z))
    {:method-type    :primary
     :dispatch-value :x
     :fn-tail        (([x] y z)
                      ([x y] z))}

    (:around :x :k [x] y z)
    {:method-type    :aux
     :qualifier      :around
     :dispatch-value :x
     :unique-key     :k
     :fn-tail        ([x] y z)}

    (:around :x :k "Docstring" [x] y z)
    {:method-type    :aux
     :qualifier      :around
     :dispatch-value :x
     :unique-key     :k
     :docstring      "Docstring"
     :fn-tail        ([x] y z)}

    (:around "str-dv" :k [x] y z)
    {:method-type    :aux
     :qualifier      :around
     :dispatch-value "str-dv"
     :unique-key     :k
     :fn-tail        ([x] y z)}

    (:around "str-dv" :k "Docstring" [x] y z)
    {:method-type    :aux
     :qualifier      :around
     :dispatch-value "str-dv"
     :unique-key     :k
     :docstring      "Docstring"
     :fn-tail        ([x] y z)}

    (:after
     :default
     ([m]
      (assoc m :after? true))
     ([x m]
      (assoc m :after? x)))
    {:method-type    :aux
     :qualifier      :after
     :dispatch-value :default
     :fn-tail        [([m]
                       (assoc m :after? true))
                      ([x m]
                       (assoc m :after? x))]}

    ;; awful dispatch values
    ([{:a 1} [2 3]] [x y] {:x x, :y y})
    {:method-type    :primary
     :dispatch-value [{:a 1} [2 3]]
     :fn-tail        [[x y] {:x x, :y y}]}

    ([{:a 1} [2 3]] ([x] {:x x, :y y}) ([x y] {:x x, :y y}))
    {:method-type    :primary
     :dispatch-value [{:a 1} [2 3]]
     :fn-tail        [([x] {:x x, :y y})
                      ([x y] {:x x, :y y})]}

    (({:a 1} [2 3]) [x y] {:x x, :y y})
    {:method-type    :primary
     :dispatch-value ({:a 1} [2 3])
     :fn-tail        [[x y] {:x x, :y y}]}

    (({:a 1} [2 3]) ([x] {:x x, :y y}) ([x y] {:x x, :y y}))
    {:method-type    :primary
     :dispatch-value ({:a 1} [2 3])
     :fn-tail        [([x] {:x x, :y y})
                      ([x y] {:x x, :y y})]}

    ;; despite looking a little ambiguous, this is actually invalid because neither `:before` nor `([x] x)` are allowed
    ;; as dispatch values; see [[methodical.macros/default-dispatch-value-spec]] for reasons why.
    [:before ([x] x) ([x y] x)]
    "([x] x) - failed: dispatch-value-spec in: [1] at: [:args-for-method-type :aux :dispatch-value]\n"

    ;; here's another ambiguous one. Is this an `:after` aux method with dispatch value `"str"`, or a primary
    ;; method with dispatch value `:after` and a docstring?
    ;;
    ;; The answer is that this is an aux method. We are no longer allowing aux qualifier keywords like `:after` as
    ;; dispatch values.
    [:after "str" [_x]]
    {:method-type    :aux
     :qualifier      :after
     :dispatch-value "str"
     :fn-tail        [[_x]]}))

(macros/defmethod mf1 :x
  [m]
  (assoc m :method :x))

(t/deftest validate-defmethod-args-test
  (t/are [invalid-form] (thrown?
                         clojure.lang.Compiler$CompilerException
                         (macroexpand (quote invalid-form)))

    ;; bad aux method
    (macros/defmethod mf1 :arounds :x
      [m]
      (assoc m :method :x))

    ;; missing function tail
    (macros/defmethod mf1 :around :x)

    ;; invalid function tail
    (macros/defmethod mf1 :around :x {} a b c)

    ;; string unique key
    (macros/defmethod mf1 :around :x "unique-key" "docstr" [a] b c)))

(s/def ::arg-validation-spec
  (s/or :default (partial = :default)
        :x-y     (s/spec (s/cat :x keyword?
                                :y keyword?))))

(macros/defmulti validate-args-spec-mf
  {:arglists '([x y]), :dispatch-value-spec ::arg-validation-spec}
  (fn [x y]
    [(keyword x) (keyword y)]))

(t/deftest validate-defmethod-dispatch-value-test
  (t/testing ":dispatch-value-spec metadata should be attached to var and to the multifn itself"
    (t/are [x] (= ::arg-validation-spec
                  (:dispatch-value-spec (meta x)))
      validate-args-spec-mf
      #'validate-args-spec-mf))
  (t/testing "valid"
    (t/are [form] (some? (macroexpand (quote form)))
      (macros/defmethod validate-args-spec-mf :default [x y])
      (macros/defmethod validate-args-spec-mf [:x :y] [x y])))

  (t/testing "invalid"
    (t/are [form] (thrown?
                   clojure.lang.Compiler$CompilerException
                   (macroexpand (quote form)))
      (macros/defmethod validate-args-spec-mf :x [x y])
      (macros/defmethod validate-args-spec-mf [:x 1] [x y])
      (macros/defmethod validate-args-spec-mf [:x] [x y])
      (macros/defmethod validate-args-spec-mf [:x :y :z] [x y]))))

(t/deftest defmethod-primary-methods-test
  (t/is (= mf1 (let [impl    (impl/standard-multifn-impl
                              (impl/thread-last-method-combination)
                              (impl/multi-default-dispatcher :type)
                              (impl/standard-method-table))
                     multifn (impl/multifn impl nil (impl/watching-cache
                                                     (impl/simple-cache)
                                                     [#'clojure.core/global-hierarchy]))]
                 (i/add-primary-method multifn :x (u/primary-method mf1 :x))))
        "A Methodical multifn defined via `defmulti` macros should be equivalent to one created using various `impl/`
      constructors.")
  (t/is (= (mf1 {:type :x})
           {:type :x, :method :x})
        "We should be able to define new primary methods using `defmethod`"))

(macros/defmulti ^:private mf2 :type)

(macros/defmethod mf2 :x
  [m]
  (assoc m :method :x))

(macros/defmethod mf2 :before :default
  [m]
  (assoc m :before? true))

(t/deftest defmethod-aux-method-test
  (t/is (= (mf2 {:type :x})
           {:type :x, :before? true, :method :x})
        "We should be able to define new aux methods using `defmethod`"))

(macros/defmulti ^:private mf3 :type)

(macros/defmethod mf3 :x
  [m]
  (assoc m :method :x))

(macros/defmethod mf3 :after :default
  [m]
  (assoc m :after? true))

;; make a var that is basically an alias for another var. This mimics the way Potemkin import-vars works
(intern *ns* 'mf4 mf3)
(alter-meta! #_{:clj-kondo/ignore [:unresolved-symbol]} #'mf4 (constantly (meta #'mf3)))

(p.namespaces/link-vars #'mf3 #'mf4)

(macros/defmethod mf4 :y
  [m]
  (assoc m :method :y))

(t/deftest defmethod-test
  (t/testing "We should be able to define new primary & aux methods using `defmethod`"
    (t/is (identical? mf3 mf4))
    (t/is (= (mf3 {:type :x})
             {:type :x, :after? true, :method :x})))

  (t/testing "`defmethod` should alter the actual var if the one it is called on is 'imported' (e.g. with Potemkin)"
    (t/is (= {:type :y, :method :y, :after? true}
             (mf3 {:type :y})
             (mf4 {:type :y})))))

(macros/defmulti multi-arity
  {:arglists '([x] [x y])}
  (fn
    ([x]
     (keyword x))
    ([x _]
     (keyword x))))

(macros/defmethod multi-arity ::wow
  ([x]
   {:x x})
  ([x y]
   {:x x, :y y}))

(macros/defmethod multi-arity :after :default
  ([m]
   (assoc m :after? true))
  ([x m]
   (assoc m :after? x)))

(macros/defmulti no-dispatch-fn)

(macros/defmethod no-dispatch-fn :first [& _]
  1)

(macros/defmethod no-dispatch-fn :second [& _]
  2)

(t/deftest multi-arity-test
  (t/testing "defmulti and defmethod with multi-arity methods (#57)"
    (t/is (= {:x ::wow, :after? true}
             (multi-arity ::wow)))
    (t/is (= {:x ::wow, :y 100, :after? ::wow}
             (multi-arity ::wow 100)))))

(t/deftest no-dispatch-fn-test
  (t/testing "not specifying a dispatch fn works (should use identity)"
    (t/is (= 1 (no-dispatch-fn :first)))
    (t/is (= 2 (no-dispatch-fn :second)))))

(macros/defmulti docstring-multifn)

(macros/defmethod docstring-multifn :docstring
  "Docstring"
  [_x])

(macros/defmethod docstring-multifn :around :docstring
  "Docstring"
  [_x])

(t/deftest docstring-test
  (t/are [x] (= "Docstring"
                (:doc (meta x)))
    (u/primary-method docstring-multifn :docstring)
    #_{:clj-kondo/ignore [:unresolved-symbol]} #'docstring-multifn-primary-method-docstring
    (first (u/aux-methods docstring-multifn :around :docstring))
    #_{:clj-kondo/ignore [:unresolved-symbol]} #'docstring-multifn-around-method-docstring))
