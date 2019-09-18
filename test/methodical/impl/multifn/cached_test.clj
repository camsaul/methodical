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
