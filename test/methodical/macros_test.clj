(ns methodical.macros-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.test :as t]
   [methodical.core :as m]
   [methodical.impl :as impl]
   [methodical.interface :as i]
   [methodical.macros :as macros]
   [methodical.util :as u]
   [potemkin.namespaces :as p.namespaces]))

(set! *warn-on-reflection* true)

;;; so the tests that do macroexansion work correctly regardless of namespace
(t/use-fixtures :each (fn [thunk]
                        (binding [*ns* (the-ns 'methodical.macros-test)]
                          (thunk))))

(defn- re-quote [^String s]
  (re-pattern (java.util.regex.Pattern/quote s)))

(defmethod t/assert-expr 'macroexpansion-spec-error? [message [_ error form]]
  (t/assert-expr
   message
   `(~'thrown-with-msg?
     Exception
     ~(re-quote error)
     (try
       (macroexpand ~form)
       (catch clojure.lang.Compiler$CompilerException e#
         (throw (or (ex-cause e#) e#)))))))

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
    (t/is (macroexpansion-spec-error?
           "Call to methodical.macros/defmulti did not conform to spec."
           '(macros/defmulti multifn :default
              [x y z]
              :ok)))))

(t/deftest method-fn-symbol-test
  (letfn [(method-fn-symbol [dispatch-value]
            (#'macros/method-fn-symbol 'my-multimethod "primary" dispatch-value))]
    (t/testing "Keyword dispatch value"
      (t/is (= 'my-multimethod-primary-method-something
               (method-fn-symbol :something)))
      (t/is (= {:private true, :multifn nil} ; `:multifn` is `nil` here because we can't calculate it since technically
                                             ; `method-fn-symbol` is supposed to be called with the multimethod itself
                                             ; rather than a symbol.
               (meta (method-fn-symbol :something)))))
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
    ;; here's another ambiguous one. Is this an `:after` aux method with dispatch value `"str"`, or a primary
    ;; method with dispatch value `:after` and a docstring?
    ;;
    ;; The answer is that this is an aux method. We are no longer allowing aux qualifier keywords like `:after` as
    ;; dispatch values.
    [:after "str" [_x]]
    {:method-type    :aux
     :qualifier      :after
     :dispatch-value "str"
     :fn-tail        [[_x]]})

  (t/testing "Errors"
    ;; despite looking a little ambiguous, this is actually invalid because neither `:before` nor `([x] x)` are allowed
    ;; as dispatch values; see [[methodical.macros/default-dispatch-value-spec]] for reasons why.
    (t/is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           (re-quote "([x] x) - failed: dispatch-value-spec in: [1] at: [:args-for-method-type :aux :dispatch-value]\n")
           (#'macros/parse-defmethod-args mf1 '[:before ([x] x) ([x y] x)])))))

(macros/defmethod mf1 :x
  [m]
  (assoc m :method :x))

(t/deftest validate-defmethod-args-test
  (t/are [invalid-form msg] (macroexpansion-spec-error? msg (quote invalid-form))

    ;; bad aux method
    (macros/defmethod mf1 :arounds :x
      [m]
      (assoc m :method :x))
    ":x - failed: vector? in: [0] at: [:fn-tail :arity-1 :params]"

    ;; missing function tail
    (macros/defmethod mf1 :around :x)
    "failed: Insufficient input at: [:fn-tail]"

    ;; invalid function tail
    (macros/defmethod mf1 :around :x {} a b c)
    "failed: Insufficient input"

    ;; string unique key
    (macros/defmethod mf1 :around :x "unique-key" "docstr" [a] b c)
    "failed: Insufficient input"))

(s/def ::arg-validation-spec
  (s/or :default (partial = :default)
        :x-y     (s/spec (s/cat :x keyword?
                                :y keyword?))))

(macros/defmulti validate-args-spec-mf
  {:arglists '([x y]), :dispatch-value-spec ::arg-validation-spec}
  (fn [x y]
    [(keyword x) (keyword y)]))

(t/deftest attach-attribute-map-metadata-to-var-test
  (t/testing "Metadata from attribute map should be attached to the var itself"
    (t/is (= '([x y])
             (:arglists (meta #'validate-args-spec-mf))))))

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
    (t/are [form msg] (macroexpansion-spec-error? msg (quote form))
      (macros/defmethod validate-args-spec-mf :x [x y])
      ":x - failed: (partial = :default) in: [0]"

      (macros/defmethod validate-args-spec-mf [:x 1] [x y])
      "failed: keyword? in: [0 1] at: [:args-for-method-type :primary :dispatch-value :x-y :y]"

      (macros/defmethod validate-args-spec-mf [:x] [x y])
      "failed: Insufficient input in: [0] at: [:args-for-method-type :primary :dispatch-value :x-y :y]"

      (macros/defmethod validate-args-spec-mf [:x :y :z] [x y])
      "failed: Extra input in: [0 2] at: [:args-for-method-type :primary :dispatch-value :x-y]")))

(t/deftest defmulti-is-equivalent-test
  (t/testing (str "A Methodical multifn defined via `defmulti` macros should be equivalent to one created using "
                  "various `impl/`constructors.")
    (let [impl (impl/standard-multifn-impl
                (impl/thread-last-method-combination)
                (impl/multi-default-dispatcher :type)
                (impl/standard-method-table))

          ^methodical.impl.standard.StandardMultiFn multifn
          (-> (impl/multifn impl nil (impl/watching-cache
                                      (impl/simple-cache)
                                      [#'clojure.core/global-hierarchy]))
              (i/add-primary-method :x (u/primary-method mf1 :x)))]
      (t/testing "Sanity check"
        (t/testing 'mf1
          (t/is (= 1
                   (count (m/primary-methods mf1)))))
        (t/testing 'multifn
          (t/is (= 1
                   (count (m/primary-methods multifn))))))
      (t/testing "impl"
        (t/testing "combo"
          (t/is (= (i/method-combination (.impl mf1))
                   (i/method-combination (.impl multifn)))))
        (t/testing "dispatcher"
          (t/is (= (i/dispatcher (.impl mf1))
                   (i/dispatcher (.impl multifn)))))
        (t/testing "method-table"
          (t/is (= (i/method-table (.impl mf1))
                   (i/method-table (.impl multifn)))))
        (t/is (= (.impl mf1)
                 (.impl multifn))))
      (t/testing "entire multifn"
        (t/is (= mf1
                 multifn))))))

(t/deftest defmethod-primary-methods-test
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


(macros/defmulti mf-dispatch-value-spec-2
  {:arglists '([x y]), :dispatch-value-spec (s/cat :x keyword?, :y int?)}
  (fn [x y] [x y]))

(t/deftest dispatch-value-spec-test-2
  (t/testing "We should specize :dispatch-value-spec if needed"
    (t/is (some?
           (macroexpand
            '(macros/defmethod mf-dispatch-value-spec-2 [:x 1]
               [x y]
               {:x x, :y y}))))
    (t/is (macroexpansion-spec-error?
           "failed: Insufficient input in: [0] at: [:args-for-method-type :primary :dispatch-value :y]"
           '(macros/defmethod mf-dispatch-value-spec-2 [:x]
              [x y]
              {:x x, :y y})))))

(t/deftest defmulti-validate-defmethod-arities-metadata-test
  (t/testing "ok"
    (t/are [arities] (some?
                      (macroexpand
                       '(macros/defmulti my-mf {:defmethod-arities arities} identity)))
      nil
      #{0}
      #{0 1 [:>= 3]}
      #{20}))

  (t/testing "bad"
    (t/are [arities] (macroexpansion-spec-error?
                      "Call to methodical.macros/defmulti did not conform to spec."
                      '(macros/defmulti my-mf {:defmethod-arities arities} identity))
      #{}
      #{-1}
      #{21}
      [0]
      {0 1}
      1
      [:>= 3]
      #{[:> 3]}
      #{:more})))

(macros/defmulti ^:private validate-arg-counts-mf
  ;; actually allowed:
  ;;
  ;; * x (1 arg)
  ;; * x y z (3 args)
  ;; * x y z :k v ... (5 + args, odd)
  ;;
  ;; 2 ARGS ARE NOT ALLOWED.
  {:arglists '([x] [x y z & {:as options}]), :defmethod-arities #{1 [:>= 3]}}
  (fn [x & _]
    x))

(t/deftest validate-defmethod-arities-test
  (t/testing "We should be able to validate arg counts in defmethod forms (#59)"
    (t/testing "valid"
      (t/are [fn-tail] (some?
                        (macroexpand
                         (list* `macros/defmethod `validate-arg-counts-mf :x (quote fn-tail))))
        [([x] :ok)
         ([x y z & more] :ok)]
        [([x] :ok)
         ([x y z & {:as options}] :ok)]
        ;; this should be ok.
        [([x] :ok)
         ([x y z] :ok)
         ([x y z & more] :ok)]
        ;; so should this one.
        [([x] :ok)
         ([x y z] :ok)
         ([x y z k] :ok)
         ([x y z k v & more] :ok)]
        ;; not sure about this one or not. TECHNICALLY this should be disallowed because it allows 4 args and we're
        ;; doing key-value destructuring... but worrying about that might be too much for us right now.
        [([x] :ok)
         ([x y z] :ok)
         ([x y z k] :ok)
         ([x y z k v & more] :ok)]))
    (t/testing "invalid"
      (t/are [fn-tail msg] (macroexpansion-spec-error?
                            msg
                            (list* `macros/defmethod `validate-arg-counts-mf :x 'fn-tail))
        ;; missing 3+
        [([x] :ok)]
        "{:arities {:required #{[:>= 3]}}}"

        [[x] :ok]
        "{:arities {:required #{[:>= 3]}}}"

        ;; missing 1
        [([x y z & {:as options}] :ok)]
        "{:arities {:required #{1}}}"

        [[x y z & {:as options}] :ok]
        "{:arities {:required #{1}}}"

        ;; missing &
        [([x] :ok) ([x y z] :ok)]
        "{:arities {:required #{[:>= 3]}}}"

        ;; 2 is not allowed
        [([x] :ok)
         ([x y] :ok)
         ([x y z & more] :ok)]
        "{:arities {:disallowed #{2}}}"

        ;; 0 is not allowed
        [([] :ok)
         ([x] :ok)
         ([x y z & more] :ok)]
        "{:arities {:disallowed #{0}}}"

        ;; This one also would theoretically work for 1, 3, or 3+ args... Not sure about this one. Should this be
        ;; allowed or not? I guess maybe not since you would be able to invoke it with 2 args which isn't allowed.
        [[x & [y z & {:as options}]]
         :ok]
        "{:arities {:disallowed #{[:>= 1]}}}"

        ;; 2 or more is not ok. We only want 3 or more.
        [([x] :ok)
         ([x y & more] :ok)]
        "{:arities {:disallowed #{[:>= 2]}}}"

        ;; not ok because while this handles 3 or 4 it doesn't handle 5+
        [([a] :ok)
         ([a b c] :ok)
         ([a b c d] :ok)]
        "{:arities {:required #{[:>= 3]}}}"

        ;; missing arities and disallowed arities
        ([x y] :ok)
        "{:arities {:required #{1 [:>= 3]}, :disallowed #{2}}}"))))

(t/deftest defmethod-methods-should-include-multifn-metadata
  (t/testing "Methods defined by defmethod should include the multifn they were defined for so we can use this info later"
    (t/is (= #'methodical.macros-test/mf1
             (:multifn (meta #_{:clj-kondo/ignore [:unresolved-symbol]} #'mf1-primary-method-x))))))

(t/deftest capture-metadata-updates-test
  (t/testing "Var/multimethod metadata should get updated when defmulti form changes (don't skip because form hasn't changed -- #129)\n"
    (letfn [(num-primary-methods []
              (count (m/primary-methods @(resolve 'methodical.macros-test/metadata-updates-mf))))]
      (ns-unmap *ns* 'metadata-updates-mf)
      (eval '(m/defmulti metadata-updates-mf {:arglists '([x])} keyword))
      (t/is (= 0
               (count (m/primary-methods @(resolve 'methodical.macros-test/metadata-updates-mf)))))
      (eval '(m/defmethod metadata-updates-mf :default [_x] :x))
      (t/is (= 1
               (num-primary-methods)))
      (t/testing "Sanity check"
        (eval '(m/defmulti metadata-updates-mf {:arglists '([x])} keyword))
        (t/is (= 1
                 (num-primary-methods))))
      (let [original-hash (::macros/defmulti-hash (meta (resolve 'methodical.macros-test/metadata-updates-mf)))
            expected-doc  ["metadata-updates-mf is defined in [[methodical.macros-test]] (methodical/macros_test.clj:574)."
                           ""
                           "It caches methods using a `methodical.impl.cache.watching.WatchingCache`."
                           ""
                           "It uses the method combination `methodical.impl.combo.threaded.ThreadingMethodCombination`"
                           "with the threading strategy `:thread-last`."
                           ""
                           "It uses the dispatcher `methodical.impl.dispatcher.multi_default.MultiDefaultDispatcher`"
                           "with hierarchy `#'clojure.core/global-hierarchy`"
                           "and prefs `{}`."
                           ""
                           "The default value is `:default`."
                           ""
                           "It uses the method table `methodical.impl.method_table.standard.StandardMethodTable`."
                           ""
                           "These primary methods are known:"
                           ""
                           "* `:default`, defined in [[methodical.macros-test]] (methodical/macros_test.clj:577) "]]
        (t/is (integer? original-hash))
        (letfn [(relevant-metadata [metadata]
                  (let [metadata (select-keys metadata [:name :private :amazing? :doc ::macros/defmulti-hash])]
                    (cond-> metadata
                      (:doc metadata) (update :doc (fn [s]
                                                     (-> s
                                                         ;; depending on who is running the tests since these actually
                                                         ;; only get defined while the test is running they might not
                                                         ;; actually have a source path...
                                                         (str/replace #"NO_SOURCE_PATH" "methodical/macros_test.clj")
                                                         str/split-lines))))))
                (var-meta []
                  (relevant-metadata (meta (resolve 'methodical.macros-test/metadata-updates-mf))))
                (multifn-meta []
                  (relevant-metadata (meta @(resolve 'methodical.macros-test/metadata-updates-mf))))]
          (t/testing "sanity check"
            (t/testing "var metadata"
              (t/is (= {:name                  'metadata-updates-mf
                        ::macros/defmulti-hash original-hash
                        :doc                   expected-doc}
                       (var-meta))))
            (t/testing "multifn metadata"
              (t/is (= {:name                  'metadata-updates-mf
                        ::macros/defmulti-hash original-hash
                        :doc                   expected-doc}
                       (multifn-meta)))))
          (t/testing "symbol metadata updated"
            (eval '(m/defmulti ^:private metadata-updates-mf {:arglists '([x])} keyword))
            (t/testing "var metadata"
              (t/is (= {:name                  'metadata-updates-mf
                        :private               true
                        ::macros/defmulti-hash original-hash
                        :doc                   expected-doc}
                       (var-meta))))
            (t/testing "multifn metadata"
              (t/is (= {:name                  'metadata-updates-mf
                        :private               true
                        ::macros/defmulti-hash original-hash
                        :doc                   expected-doc}
                       (multifn-meta))))
            (t/is (= 1
                     (num-primary-methods))))
          (t/testing "attribute map updated"
            (eval '(m/defmulti metadata-updates-mf {:arglists '([x]), :amazing? true} keyword))
            (t/testing "var metadata"
              (t/is (= {:name                  'metadata-updates-mf
                        :amazing?              true
                        ::macros/defmulti-hash original-hash
                        :doc                   expected-doc}
                       (var-meta))))
            (t/testing "multifn metadata"
              (t/is (= {:name                  'metadata-updates-mf
                        :amazing?              true
                        ::macros/defmulti-hash original-hash
                        :doc                   expected-doc}
                       (multifn-meta))))
            (t/is (= 1
                     (num-primary-methods))))
          (t/testing "docstring updated"
            (eval '(m/defmulti metadata-updates-mf "Dox" {:arglists '([x])} keyword))
            (t/testing "var metadata"
              (t/is (= {:name                  'metadata-updates-mf
                        ::macros/defmulti-hash original-hash
                        :doc                   (into ["Dox" ""] expected-doc)}
                       (var-meta))))
            (t/testing "multifn metadata"
              (t/is (= {:name                  'metadata-updates-mf
                        ::macros/defmulti-hash original-hash
                        :doc                   (into ["Dox" ""] expected-doc)}
                       (multifn-meta))))
            (t/is (= 1
                     (num-primary-methods)))))))))
