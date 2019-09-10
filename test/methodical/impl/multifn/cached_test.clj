(ns methodical.impl.multifn.cached-test
  (:require [clojure.test :refer :all]
            [methodical.core :as m]))

(deftest empty-copy-test
  (testing "adding methods"
    (let [multifn  (-> (m/default-multifn :type)
                       (m/add-primary-method Object (fn [_ x]
                                                      [:object x])))
          multifn' (m/add-primary-method multifn String (fn [_ x]
                                                          [:string x]))]
      (is (= [:object {:type String}]
             (multifn {:type String})))

      (testing "Cache multimethod for dispatch value should get cleared when adding a new method."
        (is (= [:string {:type String}]
               (multifn' {:type String})))))))
