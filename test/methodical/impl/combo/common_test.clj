(ns methodical.impl.combo.common-test
  (:require [clojure.test :as t]
            [methodical.impl.combo.common :as combo.common]))

(t/deftest combine-primary-methods-test
  (t/testing "Primary methods should get an implicit `next-method` arg when combined"
    (let [f (combo.common/combine-primary-methods
             [(fn [next-method v]
                (cond-> (conj v :primary-1) next-method next-method))
              (fn [next-method v]
                (cond-> (conj v :primary-2) next-method next-method))])]
      (t/is (= [:primary-1 :primary-2]
               (f [])))))

  (t/testing "Should be able to call `next-method` before at any point in the method body"
    (let [f (combo.common/combine-primary-methods
             [(fn [next-method v]
                (conj (cond-> v next-method next-method) :primary-1))
              (fn [next-method v]
                (conj (cond-> v next-method next-method) :primary-2))])]
      (t/is (= [:primary-2 :primary-1]
               (f [])))))

  (t/testing "`combine-primary-methods` should return `nil` if `primary-methods` is empty"
    (t/is (= nil
             (combo.common/combine-primary-methods
              [])))))

(t/deftest apply-around-methods-test
  (t/testing "Apply-around-methods with one arg"
    (let [f (combo.common/apply-around-methods
             (fn [v] (conj v :primary))
             [(fn [next-method v]
                (-> (conj v :around-1-before)
                    next-method
                    (conj :around-1-after)))
              (fn [next-method v]
                (-> (conj v :around-2-before)
                    next-method
                    (conj :around-2-after)))])]
      (t/is (= [:around-2-before :around-1-before :primary :around-1-after :around-2-after]
               (f []))
            "Around methods should be called with implicit `next-method` arg, most-specific methods first.")))

  (t/testing "apply-around-methods with multiple args"
    (let [f (combo.common/apply-around-methods
             (fn [acc a b c] (conj acc [:primary a b c]))
             [(fn [next-method acc a b c]
                (-> acc
                    (conj [:around-1-before a b c])
                    (next-method a b c)
                    (conj [:around-1-after a b c])))
              (fn [next-method acc a b c]
                (-> acc
                    (conj [:around-2-before a b c])
                    (next-method a b c)
                    (conj [:around-2-after a b c])))])]
      (t/testing "Around methods should thread their values thru to to the combined before-primary-after method."
        (t/is (= [[:around-2-before :a :b :c]
                  [:around-1-before :a :b :c]
                  [:primary :a :b :c]
                  [:around-1-after :a :b :c]
                  [:around-2-after :a :b :c]]
                 (f [] :a :b :c)))))))

(t/deftest add-implicit-next-method-args-test
  (t/testing "single-arity fn tails"
    (let [tail '([x] (inc x))]
      (doseq [qualifier [nil :around]]
        (t/testing qualifier
          (t/is (= '([next-method x] (inc x))
                   (combo.common/add-implicit-next-method-args qualifier tail)))))
      (doseq [qualifier [:before :after]]
        (t/testing qualifier
          (t/is (= tail
                   (combo.common/add-implicit-next-method-args :before tail)))))))
  (t/testing "multi-arity fn tails (#57)"
    (let [tail '(([x] (inc x))
                 ([x y] (+ x y)))]
      (doseq [qualifier [nil :around]]
        (t/testing qualifier
          (t/is (= '(([next-method x] (inc x))
                     ([next-method x y] (+ x y)))
                   (combo.common/add-implicit-next-method-args qualifier tail)))))
      (doseq [qualifier [:before :after]]
        (t/testing qualifier
          (t/is (= tail
                   (combo.common/add-implicit-next-method-args :before tail)))))))
  (t/testing "Throw Exception on invalid fn tails"
    (t/is (thrown-with-msg?
           AssertionError
           #"Assert failed:.*\(sequential\? fn-tail\)"
           (combo.common/add-implicit-next-method-args nil nil)))
    (t/is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid fn tail: \(\"ABC\" \(x y\)\)"
           (combo.common/add-implicit-next-method-args nil '("ABC" (x y)))))))
