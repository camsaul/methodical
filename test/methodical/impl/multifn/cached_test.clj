(ns methodical.impl.multifn.cached-test
  (:require [clojure.test :as t]
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

(m/defmulti f
  keyword)

(m/defmethod f :default
  [k]
  k)

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
                          (m/effective-method f Number)))))))
