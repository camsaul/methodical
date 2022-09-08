(ns methodical.impl.method-table.standard-test
  (:require [clojure.test :as t]
            [methodical.impl.method-table.standard :as standard]
            [methodical.interface :as i]))

(t/deftest print-test
  (t/is (= "(standard-method-table)"
           (pr-str (standard/->StandardMethodTable {} {})))
        "Empty method tables should print simply.")

  (t/is (= "(standard-method-table 2 primary)"
           (pr-str (standard/->StandardMethodTable {:a +, :b +} {})))
        "Method tables should print the count of primary methods if it has any.")

  (t/is (= "(standard-method-table 1 :after 3 :before)"
           (pr-str (standard/->StandardMethodTable {} {:before {:a [+] :b [+ +]}, :after {:a [+]}})))
        "Method tables should print the count of aux methods if it hash any.")

  (t/is (= "(standard-method-table 1 primary 1 :after 3 :before)"
           (pr-str (standard/->StandardMethodTable {:a +} {:before {:a [+] :b [+ +]}, :after {:a [+]}})))
        "Method tables should be able to print both primary + aux counts.")

  (t/is (= "(standard-method-table 2 :before)"
           (pr-str (standard/->StandardMethodTable {} {:before {:b [+ +]}, :after {:a []}, :around {}})))
        "Method tables shouldn't print counts aux qualifiers that are empty."))

(t/deftest add-dispatch-value-metadata-test
  (t/testing "should add ^:dispatch-value metadata to methods when you add them"
    (let [table (-> (standard/->StandardMethodTable {} {})
                    (i/add-primary-method [:x :y] 'f)
                    (i/add-aux-method :before :x 'f))]
      (t/testing "primary method"
        (t/is (= {[:x :y] 'f}
                 (i/primary-methods table)))
        (let [method (-> (i/primary-methods table) vals first)]
          (t/is (= 'f
                   method))
          (t/is (= {:dispatch-value [:x :y]}
                   (meta method)))))
      (t/testing "aux method"
        (let [method (-> (i/aux-methods table) :before vals ffirst)]
          (t/is (= {:before {:x ['f]}}
                   (i/aux-methods table)))
          (t/is (= 'f
                   method))
          (t/is (= {:dispatch-value :x}
                   (meta method))))))))
