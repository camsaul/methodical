(ns methodical.impl.combo.operator-test
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [methodical.core :as m]
   [methodical.impl.combo.operator :as combo.operator]
   [methodical.interface :as i]))

(t/deftest primary-test
  (t/testing "Empty primary methods"
    (t/testing "Combine-methods should return nil if `primary-methods` is empty."
      (t/are [operator] (= nil
                           ((combo.operator/operator operator) []))
        :+ :and :concat :do :max :min :or :seq))))

(t/deftest around-test
  (t/testing "around methods"
    (doseq [[operator primary-result combined-result] [[:+ 100 100]
                                                       [:and true true]
                                                       [:concat [:v] [:v]]
                                                       [:do :x :x]
                                                       [:max 100 100]
                                                       [:min 100 100]
                                                       [:or true true]
                                                       [:seq :v [:v]]]]
      (t/testing operator
        (let [calls   (atom [])
              primary (constantly primary-result)
              around  (fn [next-method]
                        (swap! calls conj '(around-before))
                        (let [result (next-method)]
                          (swap! calls conj (list 'around-after result))
                          result))
              f       ((combo.operator/operator operator)
                       [primary]
                       {:around [around]})]
          (t/is (= ['(around-before)
                    (list 'around-after combined-result)]
                   (do
                     (f)
                     @calls))
                "Operator method combinations should support around methods"))))))

(defn- record-calls [method-vars]
  (let [calls   (atom [])
        methods (for [varr method-vars]
                  (fn [& args]
                    (swap! calls conj (cons varr args))
                    (apply (var-get varr) args)))]
    [(partial deref calls) methods]))

(def ^:private add-1 (partial + 1))
(def ^:private add-2 (partial + 2))
(def ^:private times-2 (partial * 2))

(t/deftest do-operator-test
  (let [[calls methods] (record-calls [#'add-1 #'add-2 #'times-2])
        combined        ((combo.operator/operator :do) methods)]
    (t/testing "with one arg"
      (t/is (= 200
               (combined 100)))
      (t/is (= [[#'add-1 100]
                [#'add-2 100]
                [#'times-2 100]]
               (calls))))

    (t/testing "with no args"
      (t/is (= 2
               (combined))))

    (t/testing "with many args"
      (t/are [expected args] (= expected (apply combined args))
        8  [2 2]
        16 [2 2 2]
        32 [2 2 2 2]
        64 [2 2 2 2 2]))))

(t/deftest seq-operator-test
  (let [[calls methods] (record-calls [#'add-1 #'add-2 #'times-2])
        combined        ((combo.operator/operator :seq) methods)]
    (t/testing "with one arg"
      (t/is (= [101 102 200]
               (combined 100)))
      (t/is (= [[#'add-1 100]
                [#'add-2 100]
                [#'times-2 100]]
               (calls))))

    (t/testing "with no args"
      (t/is (= [1 2 2]
               (combined))))

    (t/testing "with many args"
      (t/are [expected args] (= expected (apply combined args))
        [5 6 8]    [2 2]
        [7 8 16]   [2 2 2]
        [9 10 32]  [2 2 2 2]
        [11 12 64] [2 2 2 2 2])))

  (t/testing "laziness"
    (let [realized?       (atom false)
          [calls methods] (record-calls (concat (repeat 10 #'add-1)
                                                (lazy-seq
                                                 (reset! realized? true)
                                                 #'add-2)))
          combined        ((combo.operator/operator :seq) methods)]
      (t/is (= [3 3 3 3 3]
               (take 5 (combined 1 1))))
      (t/is (= (repeat 5 [#'add-1 1 1])
               (calls)))
      (t/is (= false
               @realized?)))))

(defn- apply-reverse [& args]
  (reverse args))

(t/deftest concat-operator-test
  (let [[calls methods] (record-calls [#'list #'apply-reverse])
        combined        ((combo.operator/operator :concat) methods)]
    (t/testing "with one arg"
      (t/is (= [:a :b :b :a]
               (combined :a :b)))
      (t/is (= [[#'list :a :b]
                [#'apply-reverse :a :b]]
               (calls))))

    (t/testing "with no args"
      (t/is (= []
               (combined))))

    (t/testing "with many args"
      (t/are [expected args] (= expected (apply combined args))
        [:a :b :b :a] [:a :b]
        [:a :b :c :c :b :a] [:a :b :c]
        [:a :b :c :d :d :c :b :a] [:a :b :c :d]
        [:a :b :c :d :e :e :d :c :b :a] [:a :b :c :d :e] )))

  (t/testing "laziness"
    (let [realized?       (atom false)
          [calls methods] (record-calls (concat (repeat 10 #'list)
                                                (lazy-seq
                                                 (reset! realized? true)
                                                 #'reverse)))
          combined        ((combo.operator/operator :concat) methods)]
      (t/is (= [:a :b :c :a :b]
               (take 5 (combined :a :b :c))))
      (t/is (= (repeat 2 [#'list :a :b :c])
               (calls)))
      (t/is (= false
               @realized?)))))

(defn- truthy-1 [x & _] (when x :t1))
(defn- truthy-2 [x & _] (when x :t2))
(defn- falsey-1 [x & _] (when-not x :f1))
(defn- falsey-2 [x & _] (when-not x :f2))

(t/deftest and-operator-test
  (t/testing "with one arg"
    (t/testing "with all truthy results"
      (let [[calls methods] (record-calls [#'truthy-1 #'truthy-2])
            combined        ((combo.operator/operator :and) methods)]
        (t/is (= :t2
                 (combined true)))
        (t/is (= [[#'truthy-1 true] [#'truthy-2 true]]
                 (calls)))))

    (t/testing "with some true results, then a falsey result - should short-circut"
      (let [[calls methods] (record-calls [#'truthy-1 #'falsey-1 #'falsey-2 #'truthy-2])
            combined        ((combo.operator/operator :and) methods)]
        (t/is (= nil
                 (combined true)))
        (t/is (= [[#'truthy-1 true] [#'falsey-1 true]]
                 (calls)))))

    (t/testing "with all false results"
      (let [[calls methods] (record-calls [#'truthy-1 #'truthy-2])
            combined        ((combo.operator/operator :and) methods)]
        (t/is (= nil
                 (combined false)))
        (t/is (= [[#'truthy-1 false]]
                 (calls))))))

  (t/testing "with no args"
    (t/is (= true
             (((combo.operator/operator :and) [(constantly true) (constantly true)]))))

    (t/is (= false
             (((combo.operator/operator :and) [(constantly true) (constantly false) (constantly true)])))))

  (t/testing "with many args"
    (let [combined ((combo.operator/operator :and) [(fn [& args]
                                                      (concat args (reverse args)))])]
      (t/are [args expected] (= expected
                                (apply combined args))
        [:a :b]          [:a :b :b :a]
        [:a :b :c]       [:a :b :c :c :b :a]
        [:a :b :c :d]    [:a :b :c :d :d :c :b :a]
        [:a :b :c :d :e] [:a :b :c :d :e :e :d :c :b :a]  ))))

(t/deftest or-operator-test
  (t/is (= [:b :c]
           (let [f ((combo.operator/operator :or) [(complement list) list])]
             (f :b :c)))))

(t/deftest max-operator-test
  (t/is (= 100
           (((combo.operator/operator :max) (map constantly [20 40 4 100 5])) nil))))

(t/deftest min-operator-test
  (t/is (= 4
           (((combo.operator/operator :min) (map constantly [20 40 4 100 5])) nil))))

(t/deftest +-operator-test)

(t/deftest equality-test
  (t/is (= (combo.operator/->OperatorMethodCombination :do)
           (combo.operator/->OperatorMethodCombination :do)))

  (t/is (not= (combo.operator/->OperatorMethodCombination :do)
              (combo.operator/->OperatorMethodCombination :do-not))))

(t/deftest convenience-consructor-test
  (t/is (= (combo.operator/operator-method-combination :do)
           (combo.operator/->OperatorMethodCombination :do)))

  (t/is (thrown-with-msg? IllegalArgumentException #"No method"
                          (combo.operator/operator-method-combination :nope))))

(t/deftest no-methods-test
  (t/is (= nil
           (i/combine-methods (combo.operator/operator-method-combination :do) nil nil))
        "Combine-methods should return nil if there are no matching primary methods."))

(def ^:private hierarchy
  (-> (make-hierarchy)
      (derive :child :parent)
      (derive :parent :grandparent)))

(m/defmulti ^:private defmulti-do
  {:arglists '([k call-order])}
  (fn [k _] k)
  :hierarchy #'hierarchy
  :combo (combo.operator/operator-method-combination :do))

(m/defmethod defmulti-do :child
  [_ call-order]
  (swap! call-order conj :child)
  :child)

(m/defmethod defmulti-do :parent
  [_ call-order]
  (swap! call-order conj :parent)
  :parent)

(m/defmethod defmulti-do :grandparent
  [_ call-order]
  (swap! call-order conj :grandparent)
  :grandparent)

(m/defmethod defmulti-do :default
  [_ call-order]
  (swap! call-order conj :default)
  :default)

(t/deftest e2e-test
  (let [calls (atom [])]
    (t/is (= :default
             (defmulti-do :parent calls))
          ":default should be called last")
    (t/is (=
           [:parent :grandparent :default]
           @calls))))


(m/defmulti ^:private seq-multimethod
  class
  :combo (m/seq-method-combination))

(m/defmethod seq-multimethod String
  [s]
  (list 'string s))

(m/defmethod seq-multimethod Object
  [obj]
  (list 'object obj))

(m/defmethod seq-multimethod :around Object
  [x]
  (next-method (str/lower-case x)))

(t/deftest e2e-test-2
  (t/is (= '((string "wow")
             (object "wow"))
           (seq-multimethod "WOW"))
        "Test that we can use operator method combinations using the `defmulti` and `defmethod` macros."))

;; failing test for #98
(comment
  (t/deftest operator-method-combination-caching-tets
    (doseq [ks (combo/permutations [:bird :can :toucan])]
      (t/testing (vec ks)
        (let [mf (-> (m/multifn
                      (m/standard-multifn-impl
                       (m/seq-method-combination)
                       (m/standard-dispatcher
                        keyword
                        :hierarchy (atom (-> (make-hierarchy)
                                             (derive :toucan :can)
                                             (derive :toucan :bird))))
                       (m/standard-method-table)))
                     (m/add-primary-method :bird (constantly {:bird? true}))
                     (m/add-primary-method :can (constantly {:can? true}))
                     (m/prefer-method :bird :can))]
          (doseq [k ks]
            (t/testing k
              (t/is (= (case k
                         :bird   {:bird? true}
                         :can    {:can? true}
                         :toucan {:bird? true, :toucan? true})
                       (reduce merge {} (mf k)))))))))))
