(ns methodical.impl.method-table.standard-test
  (:require [clojure.test :as t]
            [methodical.impl.method-table.standard :refer [->StandardMethodTable]]))

(t/deftest print-test
  (t/is (= "(standard-method-table)"
           (pr-str (->StandardMethodTable {} {})))
        "Empty method tables should print simply.")

  (t/is (= "(standard-method-table 2 primary)"
           (pr-str (->StandardMethodTable {:a +, :b +} {})))
        "Method tables should print the count of primary methods if it has any.")

  (t/is (= "(standard-method-table 1 :after 3 :before)"
           (pr-str (->StandardMethodTable {} {:before {:a [+] :b [+ +]}, :after {:a [+]}})))
        "Method tables should print the count of aux methods if it hash any.")

  (t/is (= "(standard-method-table 1 primary 1 :after 3 :before)"
           (pr-str (->StandardMethodTable {:a +} {:before {:a [+] :b [+ +]}, :after {:a [+]}})))
        "Method tables should be able to print both primary + aux counts.")

  (t/is (= "(standard-method-table 2 :before)"
           (pr-str (->StandardMethodTable {} {:before {:b [+ +]}, :after {:a []}, :around {}})))
        "Method tables shouldn't print counts aux qualifiers that are empty."))
