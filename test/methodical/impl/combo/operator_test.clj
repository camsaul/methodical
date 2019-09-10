(ns methodical.impl.combo.operator-test
  (:require [clojure
             [string :as str]
             [test :refer :all]]
            [methodical
             [core :as m]
             [interface :as i]]
            [methodical.impl.combo.operator :as combo.operator]))

(deftest primary-test
  (are [operator] (testing operator
                    (testing "Empty primary methods"
                      (is (= nil
                             ((combo.operator/operator operator) []))
                          "Combine-methods should return nil if `primary-methods` is empty.")))
    :+ :and :concat :do :max :min :or :seq))

(deftest around-test
  (are [operator primary-result combined-result]
      (testing operator
        (testing "around methods"
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
            (is (= ['(around-before)
                    (list 'around-after combined-result)]
                   (do
                     (f)
                     @calls))
                "Operator method combinations should support around methods"))))
    :+ 100 100
    :and true true
    :concat [:v] [:v]
    :do :x :x
    :max 100 100
    :min 100 100
    :or true true
    :seq :v [:v]))

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

(deftest do-operator-test
  (let [[calls methods] (record-calls [#'add-1 #'add-2 #'times-2])
        combined        ((combo.operator/operator :do) methods)]
    (testing "with one arg"
      (is (= 200
             (combined 100)))
      (is (= [[#'add-1 100]
              [#'add-2 100]
              [#'times-2 100]]
             (calls))))

    (testing "with no args"
      (is (= 2
             (combined))))

    (testing "with many args"
      (are [expected args] (= expected (apply combined args))
        8  [2 2]
        16 [2 2 2]
        32 [2 2 2 2]
        64 [2 2 2 2 2]))))

(deftest seq-operator-test
  (let [[calls methods] (record-calls [#'add-1 #'add-2 #'times-2])
        combined        ((combo.operator/operator :seq) methods)]
    (testing "with one arg"
      (is (= [101 102 200]
             (combined 100)))
      (is (= [[#'add-1 100]
              [#'add-2 100]
              [#'times-2 100]]
             (calls))))

    (testing "with no args"
      (is (= [1 2 2]
             (combined))))

    (testing "with many args"
      (are [expected args] (= expected (apply combined args))
        [5 6 8]    [2 2]
        [7 8 16]   [2 2 2]
        [9 10 32]  [2 2 2 2]
        [11 12 64] [2 2 2 2 2])))

  (testing "laziness"
    (let [realized?       (atom false)
          [calls methods] (record-calls (concat (repeat 10 #'add-1)
                                                (lazy-seq
                                                 (reset! realized? true)
                                                 #'add-2)))
          combined        ((combo.operator/operator :seq) methods)]
      (is (= [3 3 3 3 3]
             (take 5 (combined 1 1))))
      (is (= (repeat 5 [#'add-1 1 1])
             (calls)))
      (is (= false
             @realized?)))))

(defn- apply-reverse [& args]
  (reverse args))

(deftest concat-operator-test
  (let [[calls methods] (record-calls [#'list #'apply-reverse])
        combined        ((combo.operator/operator :concat) methods)]
    (testing "with one arg"
      (is (= [:a :b :b :a]
             (combined :a :b)))
      (is (= [[#'list :a :b]
              [#'apply-reverse :a :b]]
             (calls))))

    (testing "with no args"
      (is (= []
             (combined))))

    (testing "with many args"
      (are [expected args] (= expected (apply combined args))
        [:a :b :b :a] [:a :b]
        [:a :b :c :c :b :a] [:a :b :c]
        [:a :b :c :d :d :c :b :a] [:a :b :c :d]
        [:a :b :c :d :e :e :d :c :b :a] [:a :b :c :d :e] )))

  (testing "laziness"
    (let [realized?       (atom false)
          [calls methods] (record-calls (concat (repeat 10 #'list)
                                                (lazy-seq
                                                 (reset! realized? true)
                                                 #'reverse)))
          combined        ((combo.operator/operator :concat) methods)]
      (is (= [:a :b :c :a :b]
             (take 5 (combined :a :b :c))))
      (is (= (repeat 2 [#'list :a :b :c])
             (calls)))
      (is (= false
             @realized?)))))

(defn- truthy-1 [x & _] (when x :t1))
(defn- truthy-2 [x & _] (when x :t2))
(defn- falsey-1 [x & _] (when-not x :f1))
(defn- falsey-2 [x & _] (when-not x :f2))

(deftest and-operator-test
  (testing "with one arg"
    (testing "with all truthy results"
      (let [[calls methods] (record-calls [#'truthy-1 #'truthy-2])
            combined        ((combo.operator/operator :and) methods)]
        (is (= :t2
               (combined true)))
        (is (= [[#'truthy-1 true] [#'truthy-2 true]]
               (calls)))))

    (testing "with some true results, then a falsey result - should short-circut"
      (let [[calls methods] (record-calls [#'truthy-1 #'falsey-1 #'falsey-2 #'truthy-2])
            combined        ((combo.operator/operator :and) methods)]
        (is (= nil
               (combined true)))
        (is (= [[#'truthy-1 true] [#'falsey-1 true]]
               (calls)))))

    (testing "with all false results"
      (let [[calls methods] (record-calls [#'truthy-1 #'truthy-2])
            combined        ((combo.operator/operator :and) methods)]
        (is (= nil
               (combined false)))
        (is (= [[#'truthy-1 false]]
               (calls))))))

  (testing "with no args"
    (is (= true
           (((combo.operator/operator :and) [(constantly true) (constantly true)]))))

    (is (= false
           (((combo.operator/operator :and) [(constantly true) (constantly false) (constantly true)])))))

  #_(testing "with many args"
    (are [expected args] (= expected (apply combined args))
      [:a :b :b :a] [:a :b]
      [:a :b :c :c :b :a] [:a :b :c]
      [:a :b :c :d :d :c :b :a] [:a :b :c :d]
      [:a :b :c :d :e :e :d :c :b :a] [:a :b :c :d :e] )))

(deftest or-operator-test
  (is (= [:b :c]
         (let [f ((combo.operator/operator :or) [(complement list) list])]
           (f :b :c)))))

(deftest max-operator-test
  (is (= 100
         (((combo.operator/operator :max) (map constantly [20 40 4 100 5])) nil))))

(deftest min-operator-test
  (is (= 4
         (((combo.operator/operator :min) (map constantly [20 40 4 100 5])) nil))))

(deftest +-operator-test)

(deftest equality-test
  (is (= (combo.operator/->OperatorMethodCombination :do)
         (combo.operator/->OperatorMethodCombination :do)))

  (is (not= (combo.operator/->OperatorMethodCombination :do)
            (combo.operator/->OperatorMethodCombination :do-not))))

(deftest convenience-consructor-test
  (is (= (combo.operator/operator-method-combination :do)
         (combo.operator/->OperatorMethodCombination :do)))

  (is (thrown-with-msg? IllegalArgumentException #"No method"
                        (combo.operator/operator-method-combination :nope))))

(deftest no-methods-test
  (is (= nil
         (i/combine-methods (combo.operator/operator-method-combination :do) nil nil))
      "Combine-methods should return nil if there are no matcing primary methods."))

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

(deftest e2e-test
  (let [calls (atom [])]
    (is (= :default
           (defmulti-do :parent calls))
        ":default should be called last")
    (is (=
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

(deftest e2e-test
  (is (= '((string "wow")
           (object "wow"))
         (seq-multimethod "WOW"))
      "Test that we can use operator method combinations using the `defmulti` and `defmethod` macros."))
