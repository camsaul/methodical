(ns methodical.impl.method-table.clojure-test
  (:require [clojure.test :as t]
            [methodical.impl.method-table.clojure :as method-table.clojure]))

(t/deftest print-test
  (t/is (= "(clojure-method-table)"
           (pr-str (method-table.clojure/->ClojureMethodTable {})))
        "Empty method tables should print simply.")

  (t/is (= "(clojure-method-table 2 primary)"
           (pr-str (method-table.clojure/->ClojureMethodTable {:a +, :b +})))
        "Non-empty method tables should print the number of methods in the table."))
