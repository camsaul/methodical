(ns methodical.impl.multifn.cached-test
  (:require [clojure.math.combinatorics :as combo]
            [clojure.test :as t]
            [methodical.core :as m]))

(t/deftest empty-copy-test
  (t/testing "adding methods"
    (let [multifn  (-> (m/default-multifn :type)
                       (m/add-primary-method Object (fn [_ x]
                                                      [:object x])))
          multifn' (m/add-primary-method multifn String (fn [_ x]
                                                          [:string x]))]
      (t/is (= [:object {:type String}]
               (multifn {:type String})))

      (t/testing "Cache multimethod for dispatch value should get cleared when adding a new method."
        (t/is (= [:string {:type String}]
                 (multifn' {:type String})))))))

(t/deftest dont-create-new-functions-for-the-same-effective-dispatch-value
  (t/testing (str "Reuse existing methods rather than creating new ones when dispatch values have the same effective "
                  "dispatch value (#39)")
    (t/testing "if less-specific method is cached first"
      (let [f (-> (m/default-multifn identity)
                  (m/add-primary-method :default identity))]
        (t/is (identical? (m/effective-method f Number)
                          (m/effective-method f Integer)))))
    (t/testing "if more-specific method is cached first"
      (let [f (-> (m/default-multifn identity)
                  (m/add-primary-method :default identity))]
        (t/is (identical? (m/effective-method f Integer)
                          (m/effective-method f Number)))))

    (t/testing "\nCaching should work correctly regardless of what order methods are invoked"
      (derive ::parrot ::bird)
      (doseq [dv1-permutation (combo/permutations [nil Object Number])
              dv2-permutation (combo/permutations [::bird ::parrot ::dog])]
        (let [f (-> (m/default-multifn (fn [x y] [x (:type y)]))
                    (m/add-primary-method :default (fn [_ _ m] m))
                    (m/add-aux-method :before [:default ::bird] (fn [_ m] (assoc m :bird? true)))
                    (m/add-aux-method :before [Object :default] (fn [_ m] (assoc m :object? true))))]
          (t/testing (format "\norder = %s %s" (pr-str dv1-permutation) (pr-str dv2-permutation))
            (doseq [[dv1 dv2 :as dv] (map vector dv1-permutation dv2-permutation)]
              (t/testing (format "\n dispatch value = %s" (pr-str dv))
                (let [expected (cond-> {:type dv2}
                                 (isa? dv1 Object) (assoc :object? true)
                                 (isa? dv2 ::bird) (assoc :bird? true))]
                  (t/is (= expected
                           (f dv1 {:type dv2}))))))))))))
