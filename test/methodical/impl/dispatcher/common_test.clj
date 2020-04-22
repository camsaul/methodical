(ns methodical.impl.dispatcher.common-test
  (:require [clojure.test :as t]
            [methodical.impl.dispatcher.common :as dispatcher.common]))

(t/deftest add-preference-test
  (t/is (= {:x #{:y}}
           (dispatcher.common/add-preference isa? {} :x :y)))
  (t/testing "should thrown an Exception if you try to add an illegal preference"
    (t/is (thrown-with-msg?
           IllegalStateException
           (re-pattern "Cannot prefer dispatch value :x over itself.")
           (dispatcher.common/add-preference isa? {} :x :x)))
    (t/is (thrown-with-msg?
           IllegalStateException
           (re-pattern "Preference conflict in multimethod: :x is already preferred to :y")
           (dispatcher.common/add-preference isa? {:x #{:y}} :y :x)))
    (let [h     (-> (make-hierarchy)
                    (derive :bird :animal)
                    (derive :toucan :bird))
          isa?* (partial isa? h)]
      (doseq [k [:bird :animal]]
        (t/testing (format "Prefer %s over :toucan" k)
          (t/is (thrown-with-msg?
                 IllegalStateException
                 (re-pattern (format "Preference conflict in multimethod: cannot prefer %s over its descendant :toucan."
                                     k))
                 (dispatcher.common/add-preference isa?* {} k :toucan))))))))

(t/deftest prefers-test
  (t/testing "prefers?"
    (let [h (-> (make-hierarchy)
                (derive :x :x-parent)
                (derive :y :y-parent))]
      (t/are [msg prefs] (t/testing (format "x should be preferred over y with prefers = %s" prefs)
                           (t/is (= true
                                    (dispatcher.common/prefers? h prefs :x :y))
                                 msg))
        "x is directly preferred over y"
        {:x #{:y}}
        "x is preferred over an ancestor of y"
        {:x #{:y-parent}}
        "an ancestor of x is preferred over y"
        {:x-parent #{:y}}
        "an ancestor of x is preferred over an ancestor of y"
        {:x-parent #{:y-parent}}))))

(t/deftest distinct-by-test
  (t/is (= [:a :b :c :d]
           (dispatcher.common/distinct-by identity (list :a :b :c :b :d :d))))
  (t/is (= [[:a 1] [:b 3] [:c 4] [:d 8]]
           (dispatcher.common/distinct-by first (list [:a 1] [:a 2] [:b 3] [:c 4] [:c 5] [:b 6] [:a 7] [:d 8] [:d 9])))))

;; TODO - where are the tests for `dominates?`, `domination-comparitor`, and `ambiguous?`?
