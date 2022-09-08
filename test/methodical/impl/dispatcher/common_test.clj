(ns methodical.impl.dispatcher.common-test
  (:require [clojure.test :as t]
            [methodical.impl.dispatcher.common :as dispatcher.common]))

(t/deftest prefers-test
  (t/testing "prefers?"
    (let [h (-> (make-hierarchy)
                (derive :x :x-parent)
                (derive :y :y-parent))]
      (doseq [[msg prefs] {"x is directly preferred over y"                      {:x #{:y}}
                           "x is preferred over an ancestor of y"                {:x #{:y-parent}}
                           "an ancestor of x is preferred over y"                {:x-parent #{:y}}
                           "an ancestor of x is preferred over an ancestor of y" {:x-parent #{:y-parent}}}]
        (t/testing (format "x should be preferred over y with prefers = %s" prefs)
          (t/is (= true
                   (dispatcher.common/prefers? h prefs :x :y))
                msg))))))

(t/deftest distinct-by-test
  (t/is (= [:a :b :c :d]
           (dispatcher.common/distinct-by identity (list :a :b :c :b :d :d))))
  (t/is (= [[:a 1] [:b 3] [:c 4] [:d 8]]
           (dispatcher.common/distinct-by first
                                          (list [:a 1] [:a 2] [:b 3] [:c 4] [:c 5] [:b 6] [:a 7] [:d 8] [:d 9])))))

(t/deftest dominates?-test
  (derive ::parrot ::bird)
  (derive ::parakeet ::parrot)
  (derive ::budgie ::parakeet)
  (derive ::love-bird ::parrot)
  (let [h     @#'clojure.core/global-hierarchy
        prefs {:x #{:y}}]
    (doseq [[arity dominates?] {4 (partial dispatcher.common/dominates? h prefs)
                                5 (partial dispatcher.common/dominates? h prefs ::default)}]
      (t/testing (format "%d-arity" arity)
        (t/testing "The same"
          (t/is (not (dominates? :a :b)))
          (t/is (not (dominates? ::bird ::bird))))
        (t/testing "No relation"
          (t/is (not (dominates? :a :b))))
        (t/testing "no relation, but a preference"
          (t/is (dominates? :x :y))
          (t/is (not (dominates? :y :x))))
        (t/testing "default dispatch value"
          (case (long arity)
            4 (t/is (not (dominates? ::bird ::default)))
            5 (t/is (dominates? ::bird ::default)))
          (t/is (not (dominates? ::default ::bird))))
        (t/testing "child"
          (t/is (dominates? ::parrot ::bird))
          (t/is (not (dominates? ::bird ::parrot))))
        (t/testing "indirect descendant"
          (t/is (dominates? ::budgie ::bird))
          (t/is (not (dominates? ::bird ::budgie))))
        (t/testing "siblings"
          (t/is (not (dominates? ::parakeet ::love-bird)))
          (t/is (not (dominates? ::love-bird ::parakeet))))
        (t/testing "same common ancestor, but not siblings"
          (t/is (not (dominates? ::love-bird ::budgie)))
          (t/is (not (dominates? ::budgie ::love-bird))))
        (t/testing "composite dispatch value"
          (t/testing "The same"
            (t/is (not (dominates? [Object ::parrot] [Object ::parrot]))))
          (t/testing "first value more specific"
            (t/is (dominates? [String ::parrot] [Object ::parrot]))
            (t/is (not (dominates? [Object ::parrot] [String ::parrot]))))
          (t/testing "second value more specific"
            (t/is (dominates? [Object ::parrot] [Object ::bird]))
            (t/is (not (dominates? [Object ::bird] [Object ::parrot])))
            (t/is (not (dominates? [Object ::parrot] [Object ::parrot]))))
          (t/testing "Mixed-specificity -- neither dispatch value should dominate."
            (t/is (not (dominates? [String ::bird] [Object ::parrot])))
            (t/is (not (dominates? [Object ::parrot] [String ::bird]))))
          (t/testing "Default dispatch value"
            (case (long arity)
              4 (t/is (not (dominates? [String ::bird] ::default)))
              5 (t/is (dominates? [String ::bird] ::default)))
            (t/is (not (dominates? ::default [Object ::parrot])))))))))

;; TODO - add tests for `domination-comparator`, and `ambiguous?`?
