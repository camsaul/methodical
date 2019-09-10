(ns methodical.impl.standard-test
  (:require [clojure.test :refer :all]
            [methodical
             [core :as m]
             [impl :as impl]
             [interface :as i]]
            [methodical.impl.standard :as impl.standard]))

(deftest print-test
  (is (= (pr-str '(multifn
                   (cached-multifn-impl
                    (standard-multifn-impl
                     (threading-method-combination :thread-last)
                     (standard-dispatcher dispatch-fn)
                     (standard-method-table))
                    (simple-cache))))
         (pr-str (impl/multifn (impl/default-multifn-impl 'dispatch-fn))))
      "Multifns and their component parts should print in a readable format."))

(deftest equality-test
  (is (= (impl/multifn (impl/clojure-multifn-impl keyword))
         (impl/multifn (impl/clojure-multifn-impl keyword)))
      "Two mulitfns with the same impls should be equal")

  (is (= (impl/multifn (impl/clojure-multifn-impl keyword) {:metadata? true})
         (impl/multifn (impl/clojure-multifn-impl keyword)))
      "Two mulitfns with the same impls and different metadata should still be equal"))

(deftest metadata-test
  (let [a-multifn (impl/multifn (impl/default-multifn-impl keyword) {:metadata? true})]
    (is (= {:metadata? true}
           (meta a-multifn))
        "Multifns should implement clojure.lang.IMeta, and you should be able to pass metadata to `impl/multifn`.")

    (is (= {:new-metadata? true}
           (meta (with-meta a-multifn {:new-metadata? true})))
        "Multifns should implement clojure.lang.IObj, and you should be able to change their metadata.")))

(deftest name-test
  (let [a-multifn (impl/multifn (impl/default-multifn-impl keyword) {:name 'my-multifn, :ns *ns*})]
    (is (= "my-multifn"
           (name a-multifn))
        "Multifns should implement clojure.lang.Named, and return the `:name` in their metadata.")

    (is (= (name (ns-name *ns*))
           (namespace a-multifn))
        "Multifns should return `:namespace` in their metadata when calling `namespace`."))

  (let [anonymous-multifn (impl/multifn (impl/default-multifn-impl keyword))]
    (is (= nil
           (name anonymous-multifn))
        "Anonymous multifns should return `nil` when asked their `name`.")

    (is (= nil
           (namespace anonymous-multifn))
        "Anonymous multifns should return `nil` when asked their `namespace`.")))

(deftest invoke-test
  (let [f (-> (impl.standard/->StandardMultiFn (m/default-multifn-impl (constantly :key)) nil)
              (m/add-primary-method :key (fn [_ & args]
                                           (count args))))]
    (testing "invoke"
      (are [expected-count fn-call] (testing (format "invoking multifn with %d args" expected-count)
                                      (is (= expected-count
                                             fn-call)))

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

    (testing "apply"
      (is (= 5
             (apply f '[a b c d e]))))))

(deftest method-table-test
  (let [f (impl.standard/->StandardMultiFn (m/default-multifn-impl (constantly :key)) nil)]
    (testing "method-table"
      (is (= (m/standard-method-table)
             (m/method-table f))
          "You should be able to access the method table object by calling `method-table`"))

    (testing "with-method-table"
      (let [f' (m/with-method-table f (m/standard-method-table {:key 'm} {}))]
        (is (= (m/standard-method-table {:key 'm} {})
               (m/method-table f'))
            "You should be able to use a new method table by calling with-method-table")

        (is (= (m/standard-method-table)
               (m/method-table f))
            "with-method-table should return a copy, and leave the original unchanged")

        (testing "optimizations"
          (let [f'' (m/with-method-table f' (m/standard-method-table {:key 'm} {}))]
            (is (identical? f' f'')
                "with-method-table should only return a new object if the method table is not=")))

        (testing "preconditions"
          (is (thrown-with-msg? AssertionError #"Assert failed" (m/with-method-table f {:key 'm}))
              "with-method-table should throw an Exception if called with something that's not a MethodTable")))))

  (testing "adding methods"
    (let [multifn  (-> (m/default-multifn :type)
                       (m/add-primary-method Object (fn [_ x]
                                                      [:object x])))
          multifn' (m/add-primary-method multifn String (fn [_ x]
                                                          [:string x]))]
      (testing "Adding a primary method should not affect the original method"
        (is (not= multifn
                  multifn'))

        (is (= [:object {:type String}]
               (multifn {:type String})))

        (is (= [:string {:type String}]
               (multifn' {:type String}))
            "Adding a primary method should affect the copy that is returned")))))

(deftest dispatcher-test
  (let [dispatch-fn (constantly :key)
        f           (impl.standard/->StandardMultiFn (m/default-multifn-impl dispatch-fn) nil)]
    (testing "dispatcher"
      (is (= (m/standard-dispatcher dispatch-fn)
             (i/dispatcher f))))

    (testing "with-dispatcher"
      (let [dispatch-fn-2 (constantly :key-2)
            f'            (i/with-dispatcher f (m/standard-dispatcher dispatch-fn-2))]
        (is (= (m/standard-dispatcher dispatch-fn-2)
               (i/dispatcher f'))
            "You should be able to change the dispatcher with with-dispatcher")

        (is (not= f
                  f')
            "with-dispatcher should return a copy")

        (testing "optimizations"
          (if (= f'
                 (i/with-dispatcher f' (m/standard-dispatcher dispatch-fn-2)))
            "with-dispatcher should only return a new object if the dispatcher is not="))

        (testing "preconditions"
          (is (thrown-with-msg? AssertionError #"Assert failed" (i/with-dispatcher f (fn [])))
              "with-dispatcher should throw an Exception if called with something that's not a Dispatcher")))))

  (testing "dispatch-value"
    (is (= ::my-type
           (i/dispatch-value (m/default-multifn :type) {:type ::my-type})))))

(deftest method-combination-test
  (let [f (impl.standard/->StandardMultiFn (m/default-multifn-impl (constantly :key)) nil)]
    (is (= (m/thread-last-method-combination)
           (i/method-combination f)))))

(deftest dispatch-value-test
  (let [f (impl.standard/->StandardMultiFn (m/default-multifn-impl (fn [& args] (count args))) nil)]
    (are [num-args fn-call] (testing (format "dispatch-value w/ %d args" num-args)
                              (is (= num-args
                                     fn-call)))
      0 (i/dispatch-value f)
      1 (i/dispatch-value f :a)
      2 (i/dispatch-value f :a :b)
      3 (i/dispatch-value f :a :b :c)
      4 (i/dispatch-value f :a :b :c :d)
      5 (i/dispatch-value f :a :b :c :d [:e])
      6 (i/dispatch-value f :a :b :c :d [:e :f]))))
