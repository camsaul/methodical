(ns methodical.impl.standard-test
  (:require [clojure.test :as t]
            [methodical.core :as m]
            [methodical.impl :as impl]
            [methodical.impl.standard :as impl.standard]
            [methodical.interface :as i]))

(t/deftest print-test
  (t/is (= (pr-str '(multifn
                     (cached-multifn-impl
                      (standard-multifn-impl
                       (threading-method-combination :thread-last)
                       (multi-default-dispatcher dispatch-fn)
                       (standard-method-table))
                      (simple-cache))))
           (pr-str (impl/multifn (impl/default-multifn-impl 'dispatch-fn))))
        "Multifns and their component parts should print in a readable format."))

(t/deftest equality-test
  (t/is (= (impl/multifn (impl/clojure-multifn-impl keyword))
           (impl/multifn (impl/clojure-multifn-impl keyword)))
        "Two mulitfns with the same impls should be equal")

  (t/is (= (impl/multifn (impl/clojure-multifn-impl keyword) {:metadata? true})
           (impl/multifn (impl/clojure-multifn-impl keyword)))
        "Two mulitfns with the same impls and different metadata should still be equal"))

(t/deftest metadata-test
  (let [a-multifn (impl/multifn (impl/default-multifn-impl keyword) {:metadata? true})]
    (t/is (= {:metadata? true}
             (meta a-multifn))
          "Multifns should implement clojure.lang.IMeta, and you should be able to pass metadata to `impl/multifn`.")

    (t/is (= {:new-metadata? true}
             (meta (with-meta a-multifn {:new-metadata? true})))
          "Multifns should implement clojure.lang.IObj, and you should be able to change their metadata.")))

(t/deftest name-test
  (let [a-multifn (impl/multifn (impl/default-multifn-impl keyword) {:name 'my-multifn, :ns *ns*})]
    (t/is (= "my-multifn"
             (name a-multifn))
          "Multifns should implement clojure.lang.Named, and return the `:name` in their metadata.")

    (t/is (= (name (ns-name *ns*))
             (namespace a-multifn))
          "Multifns should return `:namespace` in their metadata when calling `namespace`."))

  (let [anonymous-multifn (impl/multifn (impl/default-multifn-impl keyword))]
    (t/is (= nil
             (name anonymous-multifn))
          "Anonymous multifns should return `nil` when asked their `name`.")

    (t/is (= nil
             (namespace anonymous-multifn))
          "Anonymous multifns should return `nil` when asked their `namespace`.")))

(t/deftest invoke-test
  (let [f (-> (impl.standard/->StandardMultiFn (m/default-multifn-impl (constantly :key)) nil)
              (m/add-primary-method :key (fn [_ & args]
                                           (count args))))]
    (t/testing "invoke"
      (t/are [expected-count fn-call] (= expected-count fn-call)

        0 (f)
        1 (f :a)
        2 (f :a :b)
        3 (f :a :b :c)
        4 (f :a :b :c :d)
        5 (f :a :b :c :d :e)
        6 (f :a :b :c :d :e :f)
        7 (f :a :b :c :d :e :f :g)
        8 (f :a :b :c :d :e :f :g :h)
        9 (f :a :b :c :d :e :f :g :h :i)
        10 (f :a :b :c :d :e :f :g :h :i :j)
        11 (f :a :b :c :d :e :f :g :h :i :j :k)
        12 (f :a :b :c :d :e :f :g :h :i :j :k :l)
        13 (f :a :b :c :d :e :f :g :h :i :j :k :l :m)
        14 (f :a :b :c :d :e :f :g :h :i :j :k :l :m :n)
        15 (f :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o)
        16 (f :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p)
        17 (f :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q)
        18 (f :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r)
        19 (f :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s)
        20 (f :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t)
        21 (f :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u)
        22 (f :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v)))

    (t/testing "apply"
      (t/is (= 5
               (apply f '[a b c d e]))))))

(t/deftest method-table-test
  (let [f (impl.standard/->StandardMultiFn (m/default-multifn-impl (constantly :key)) nil)]
    (t/testing "method-table"
      (t/is (= (m/standard-method-table)
               (m/method-table f))
            "You should be able to access the method table object by calling `method-table`"))

    (t/testing "with-method-table"
      (let [f' (m/with-method-table f (m/standard-method-table {:key 'm} {}))]
        (t/is (= (m/standard-method-table {:key 'm} {})
                 (m/method-table f'))
              "You should be able to use a new method table by calling with-method-table")

        (t/is (= (m/standard-method-table)
                 (m/method-table f))
              "with-method-table should return a copy, and leave the original unchanged")

        (t/testing "optimizations"
          (let [f'' (m/with-method-table f' (m/standard-method-table {:key 'm} {}))]
            (t/is (identical? f' f'')
                  "with-method-table should only return a new object if the method table is not=")))

        (t/testing "preconditions"
          (t/is (thrown-with-msg? AssertionError #"Assert failed" (m/with-method-table f {:key 'm}))
                "with-method-table should throw an Exception if called with something that's not a MethodTable")))))

  (t/testing "adding methods"
    (let [multifn  (-> (m/default-multifn :type)
                       (m/add-primary-method Object (fn [_ x]
                                                      [:object x])))
          multifn' (m/add-primary-method multifn String (fn [_ x]
                                                          [:string x]))]
      (t/testing "Adding a primary method should not affect the original method"
        (t/is (not= multifn
                    multifn'))

        (t/is (= [:object {:type String}]
                 (multifn {:type String})))

        (t/is (= [:string {:type String}]
                 (multifn' {:type String}))
              "Adding a primary method should affect the copy that is returned")))))

(t/deftest dispatcher-test
  (let [dispatch-fn (constantly :key)
        f           (impl.standard/->StandardMultiFn (m/default-multifn-impl dispatch-fn) nil)]
    (t/testing "dispatcher"
      (t/is (= (m/multi-default-dispatcher dispatch-fn)
               (i/dispatcher f))))

    (t/testing "with-dispatcher"
      (let [dispatch-fn-2 (constantly :key-2)
            f'            (i/with-dispatcher f (m/standard-dispatcher dispatch-fn-2))]
        (t/is (= (m/standard-dispatcher dispatch-fn-2)
                 (i/dispatcher f'))
              "You should be able to change the dispatcher with with-dispatcher")

        (t/is (not= f
                    f')
              "with-dispatcher should return a copy")

        (t/testing "optimizations"
          (t/testing "with-dispatcher should only return a new object if the dispatcher is not="
            (t/is (= f'
                     (i/with-dispatcher f' (m/standard-dispatcher dispatch-fn-2))))))

        (t/testing "preconditions"
          (t/is (thrown-with-msg? AssertionError #"Assert failed" (i/with-dispatcher f (fn [])))
                "with-dispatcher should throw an Exception if called with something that's not a Dispatcher")))))

  (t/testing "dispatch-value"
    (t/is (= ::my-type
             (i/dispatch-value (m/default-multifn :type) {:type ::my-type})))))

(t/deftest method-combination-test
  (let [f (impl.standard/->StandardMultiFn (m/default-multifn-impl (constantly :key)) nil)]
    (t/is (= (m/thread-last-method-combination)
             (i/method-combination f)))))

(t/deftest dispatch-value-test
  (let [f (impl.standard/->StandardMultiFn (m/default-multifn-impl (fn [& args] (count args))) nil)]
    (t/are [num-args fn-call] (= num-args fn-call)
      0 (i/dispatch-value f)
      1 (i/dispatch-value f :a)
      2 (i/dispatch-value f :a :b)
      3 (i/dispatch-value f :a :b :c)
      4 (i/dispatch-value f :a :b :c :d)
      5 (i/dispatch-value f :a :b :c :d [:e])
      6 (i/dispatch-value f :a :b :c :d [:e :f]))))
