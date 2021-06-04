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
      (derive ::parakeet ::parrot)
      (doseq [permutation (combo/permutations [::bird ::parrot ::parakeet ::dog])]
        (let [f (-> (m/default-multifn :type)
                    (m/add-primary-method :default (fn [_ m] m))
                    (m/add-aux-method :after ::bird #(assoc % :bird? true))
                    (m/add-aux-method :after ::parrot #(assoc % :parrot? true)))]
          (t/testing (format "\norder = %s" (mapv name permutation))
            (doseq [v permutation]
              (t/testing (format "\n dispatch value = %s" (name v))
                (let [expected (case v
                                 ::bird     {:type ::bird, :bird? true}
                                 ::parrot   {:type ::parrot, :bird? true, :parrot? true}
                                 ::parakeet {:type ::parakeet, :bird? true, :parrot? true}
                                 ::dog      {:type ::dog})]
                  (t/is (= expected
                           (f {:type v}))))))))))))
