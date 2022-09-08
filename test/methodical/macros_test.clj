(ns methodical.macros-test
  (:require [clojure.test :as t]
            [methodical.impl :as impl]
            [methodical.interface :as i]
            [methodical.macros :as m]
            [methodical.util :as u]
            [potemkin.namespaces :as p.namespaces]))

(t/deftest method-fn-name-test
  (letfn [(method-fn-name [dispatch-value]
            (#'m/method-fn-name 'my-multimethod "primary" dispatch-value))]
    (t/testing "Keyword dispatch value"
      (t/is (= 'my-multimethod-primary-method-something
               (method-fn-name :something))))
    (t/testing "Symbol dispatch value"
      (t/is (= 'my-multimethod-primary-method-String
               (method-fn-name 'String)))
      (t/testing "with namespace"
        (t/is (= 'my-multimethod-primary-method-somewhere-something-cool
                 (method-fn-name 'somewhere/something.cool)))))
    (t/testing "String dispatch value"
      (t/is (= 'my-multimethod-primary-method-Wow-OK
               (method-fn-name "Wow OK"))))
    (t/testing "Composite dispatch value"
      (t/is (= 'my-multimethod-primary-method-k1-String
               (method-fn-name [:k1 'String]))))
    (t/testing "Arbitrary expression dispatch value"
      (t/is (= 'my-multimethod-primary-method-if-true-String-Integer
               (method-fn-name '[(if true String Integer)]))))
    (t/testing "Munged function name should include keyword namespaces"
      (t/is (= 'my-multimethod-primary-method-somewhere-something
               (method-fn-name :somewhere/something))))
    (t/testing "Munged function name should replace periods in keywords (#60)"
      (t/testing "Munged function name should include keyword namespaces"
        (t/is (= 'my-multimethod-primary-method-somewhere-else-something
                 (method-fn-name :somewhere.else/something)))
        (t/is (= 'my-multimethod-primary-method-somewhere-something-else
                 (method-fn-name :somewhere/something-else)))))
    (t/testing "Illegal characters"
      (t/is (= 'my-multimethod-primary-method-can_SINGLEQUOTE_t-use-this
               (method-fn-name "can't use this"))))))

(m/defmulti ^:private mf1 :type)

(m/define-primary-method mf1 :x
  [m]
  (assoc m :method :x))

(t/deftest macros-test
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
        "We should be able to define new primary methods using `define-primary-method`"))

(m/defmulti ^:private mf2 :type)

(m/define-primary-method mf2 :x
  [m]
  (assoc m :method :x))

(m/define-aux-method  mf2 :before :default
  [m]
  (assoc m :before? true))

(t/deftest define-aux-method-test
  (t/is (= (mf2 {:type :x})
           {:type :x, :before? true, :method :x})
        "We should be able to define new aux methods using `define-aux-method`"))

(m/defmulti ^:private mf3 :type)

(m/defmethod mf3 :x
  [m]
  (assoc m :method :x))

(m/defmethod mf3 :after :default
  [m]
  (assoc m :after? true))

;; make a var that is basically an alias for another var. This mimics the way Potemkin import-vars works
(intern *ns* 'mf4 mf3)
(alter-meta! #_{:clj-kondo/ignore [:unresolved-symbol]} #'mf4 (constantly (meta #'mf3)))

(p.namespaces/link-vars #'mf3 #'mf4)

(m/defmethod mf4 :y
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

(m/defmulti multi-arity
  {:arglists '([x] [x y])}
  (fn
    ([x]
     (keyword x))
    ([x _]
     (keyword x))))

(m/defmethod multi-arity ::wow
  ([x]
   {:x x})
  ([x y]
   {:x x, :y y}))

(m/defmethod multi-arity :after :default
  ([m]
   (assoc m :after? true))
  ([x m]
   (assoc m :after? x)))

(m/defmulti no-dispatch-fn)

(m/defmethod no-dispatch-fn :first [& _]
  1)

(m/defmethod no-dispatch-fn :second [& _]
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
