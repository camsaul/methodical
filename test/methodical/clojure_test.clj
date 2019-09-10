(ns methodical.clojure-test
  "Tests to ensure we can replicate the basic behavior of vanilla Clojure multimethods."
  (:require [clojure.test :refer :all]
            [methodical
             [impl :as impl]
             [interface :as i]]))

(defn- clojure-multifn [dispatch-fn & options]
  (impl/multifn (apply impl/clojure-multifn-impl dispatch-fn options)))

(defn- add-methods [multifn fns]
  (reduce
   (fn [multifn [dispatch-val f]]
     (i/add-primary-method multifn dispatch-val f))
   multifn
   fns))

(deftest basic-test
  ;; basic multifn with a few method impls, including a default
  (let [multifn (-> (clojure-multifn keyword)
                    (add-methods {:x       (fn [k] {k true})
                                  :y       (fn [k] {k false})
                                  :default (fn [k] {k :default})}))]
    (are [arg result] (= (multifn arg) result)
      :x {:x true}
      :y {:y false}
      :z {:z :default}
      "x" {"x" true})))

(deftest no-matching-method-test
  (let [multifn (clojure-multifn keyword)]
    (is (thrown-with-msg? UnsupportedOperationException #"No matching method for dispatch value :x" (multifn :x))
        "Multifns with no default implementation should throw an Exception")))

(deftest custom-default-value-test
  (let [multifn (-> (clojure-multifn keyword :default-value ::default)
                    (add-methods {:default  (constantly :wrong-default)
                                  ::default (constantly :correct-default)}))]
    (is (= (multifn :x) :correct-default)
        "Should support custom default values")))

(deftest multi-arg-multifn-test
  (let [multifn (-> (clojure-multifn (fn [operator _ _] (keyword operator)))
                    (add-methods {:+ (fn [_ x y] (+ x y))}))]
    (is (= (multifn :+ 1 2) 3)
        "Should support multifns with 2 args, dispatching on first arg")))

(deftest varargs-multifn-test
  (let [multifn (-> (clojure-multifn (fn [operator & _] (keyword operator)))
                    (add-methods {:+ (fn [_ & args] (apply + args))}))]
    (is (= (multifn :+ 1 2) 3)
        "Should support vararg multifns called with 2 args")
    (is (= (multifn :+ 1 2 3) 6)
        "Should support vararg multifns called with 3 args")
    (is (= (multifn :+ 1 2 3 4 5 6) 21)
        "Should support vararg multifns called with more than four args")
    (is (= (apply multifn :+ (range 0 100)) 4950)
        "Should be able to apply large number of args to a multifn")))

(deftest multiple-dispatch-test
  (let [multifn (-> (clojure-multifn (fn [k1 k2 _]
                                       [(keyword k1) (keyword k2)]))
                    (add-methods {[:a :x]  (fn [_ _ v] [:a :x v])
                                  [:a :y]  (fn [_ _ v] [:a :y v])
                                  [:b :x]  (fn [_ _ v] [:b :x v])
                                  :default (fn [_ _ v] [:default v])}))]
    (are [k1 k2 result] (= (multifn k1 k2 1) result)
      :a :x [:a :x 1]
      :a :y [:a :y 1]
      :a :z [:default 1])))

(deftest falsey-dispatch-value-test
  (let [multifn (-> (clojure-multifn identity)
                    (add-methods {nil   (constantly :nil)
                                  false (constantly :false)}))]
    (is (= (multifn false) :false)
        "Should handle methods with `false` dispatch values")
    (is (= (multifn nil) :nil)
        "Should handle methods with `nil` dispatch values")))

(deftest nil-default-value-test
  (let [multifn (-> (clojure-multifn identity, :default-value nil)
                    (add-methods {nil (constantly :default)}))]
    (is (= (multifn :x) :default)
        "Should properly handle nil default values")))

(deftest no-arg-multifn-test
  (with-local-vars [varr nil]
    (let [multifn (-> (clojure-multifn (fn [] (var-get varr)))
                      (add-methods {:x       (constantly :x)
                                    :y       (constantly :y)
                                    :default (constantly :default)}))]
      (are [bound-val result] (= result (with-bindings {varr bound-val} (multifn)))
        :x :x
        :y :y
        :z :default))))

(deftest multi-arity-dispatch-fn-test
  (with-local-vars [varr nil]
    (let [multifn (-> (clojure-multifn (fn
                                         ([] (var-get varr))
                                         ([x] x)))
                      (add-methods {:x       (constantly :x)
                                    :y       (constantly :y)
                                    :default (constantly :default)}))]
      (are [bound-val result] (= (with-bindings {varr bound-val} (multifn)) result)
        :x :x
        :z :default)
      (are [bound-val arg result] (= (with-bindings {varr bound-val} (multifn arg)) result)
        :x :y :y
        :x :z :default))))

(deftest hierarchy-test
  (with-local-vars [hierarchy (-> (make-hierarchy)
                                  (derive :child :parent)
                                  (derive :another-child :parent)
                                  (derive :parent :grandparent))]
    (let [multifn (-> (clojure-multifn keyword :hierarchy hierarchy)
                      (add-methods {:parent      (constantly :parent)
                                    :grandparent (constantly :grandparent)
                                    :default     (constantly :default)}))]
      (are [arg result] (= (multifn arg) result)
        :child         :parent
        :parent        :parent
        :another-child :parent
        :grandparent   :grandparent
        :cousin        :default))))

(defn- make-ambiguous-hierarchy []
  (-> (make-hierarchy)
      (derive :child :parent-1)
      (derive :child :parent-2)))

(defn- ambiguous-hierarchy-multifn [hierarchy]
  (-> (clojure-multifn keyword :hierarchy hierarchy)
      (add-methods {:parent-1 (constantly :parent-1)
                    :parent-2 (constantly :parent-2)})))

(deftest ambiguous-dispatch-test
  (with-local-vars [hierarchy (make-ambiguous-hierarchy)]
    (let [multifn (ambiguous-hierarchy-multifn hierarchy)]
      (testing "Ambiguous invocation"
        (is (thrown-with-msg? IllegalArgumentException
                              #"Multiple methods match dispatch value: :child -> :parent-1 and :parent-2"
                              (multifn :child))
            "Should throw Exception if multiple methods match dispatch value"))

      (testing "Ambiguous ancestor values"
        (let [multifn (add-methods multifn {:child (constantly :child)})]
          (is (= (multifn :child) :child)
              "Ambiguous ancestor values should be ignored"))))))

(deftest prefer-method-test
  (with-local-vars [hierarchy (make-ambiguous-hierarchy)]
    (let [multifn (-> (ambiguous-hierarchy-multifn hierarchy)
                      (i/prefer-method :parent-1 :parent-2))]
      (is (= (i/prefers multifn) {:parent-1 #{:parent-2}})
          "Map of prefers should be visible by calling `prefers`")
      (is (= (multifn :child) :parent-1)
          "It should be possible to prefer one ambiguous method over another"))))

(deftest prefer-method-validation-test
  (with-local-vars [hierarchy (make-ambiguous-hierarchy)]
    (let [multifn (ambiguous-hierarchy-multifn hierarchy)]
      (is (thrown-with-msg? IllegalStateException
                            #"Cannot prefer dispatch value :parent-1 over itself"
                            (i/prefer-method multifn :parent-1 :parent-1))
          "Trying to prefer a dispatch value over itself should throw an Exception")
      (is (thrown-with-msg? IllegalStateException
                            #"Preference conflict in multimethod: :parent-1 is already preferred to :parent-2"
                            (-> multifn
                                (i/prefer-method :parent-1 :parent-2)
                                (i/prefer-method :parent-2 :parent-1)))
          "You should not be able to prefer something if it would conflict with an existing prefer")
      (is (thrown-with-msg? IllegalStateException
                            #"Preference conflict in multimethod: cannot prefer :parent-1 over its descendant :child"
                            (i/prefer-method multifn :parent-1 :child))
          "You should not be able to prefer an ancestor over its descendant."))))

(deftest remove-primary-method-test
  (let [a       (constantly :a)
        default (constantly :default)
        multifn (-> (clojure-multifn keyword)
                    (add-methods {:a a, :default default}))]
    (testing "sanity check"
      (is (= (i/primary-methods multifn) {:a a, :default default})
          "You should be able to see a map of dispatch-value -> primary method with `primary-methods`"))
    (testing "remove-primary-method"
      (let [multifn (i/remove-primary-method multifn :a)]
        (is (= (i/primary-methods multifn) {:default default}))
        (is (= (multifn :a) :default))))))

(deftest effective-method-test
  (let [a-method       (fn [v] {:a v})
        default-method (fn [v] {:default v})
        multifn        (-> (clojure-multifn keyword)
                           (add-methods {:a       a-method
                                         :default default-method}))]
    (is (= a-method (i/effective-method multifn :a))
        "For Clojure-style multifns, `effective-method` should work just like vanilla `get-method`.")
    (is (= default-method (i/effective-method multifn :b))
        "The default method should be returned if no matching method is found.")
    (let [multifn (i/remove-primary-method multifn :default)]
      (is (= nil (i/effective-method multifn :b))
          "If no default method exists, `effective-method` should return nil if no methods match."))))

;; TODO - test other methods not available in vanilla Clojure, e.g. `dominates?` and `aux-methods`
