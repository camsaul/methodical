(ns methodical.impl.method-table.clojure-test
  (:require [clojure.test :refer :all]
            [methodical.impl.method-table.clojure :refer [->ClojureMethodTable]]))

(deftest print-test
  (is (= "(clojure-method-table)"
         (pr-str (->ClojureMethodTable {})))
      "Empty method tables should print simply.")

  (is (= "(clojure-method-table 2 primary)"
         (pr-str (->ClojureMethodTable {:a +, :b +})))
      "Non-empty method tables should print the number of methods in the table."))
