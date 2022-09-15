(ns methodical.impl.method-table.common-test
  (:require
   [clojure.test :as t]
   [methodical.impl.method-table.common :as method-table.common]))

(t/deftest describe-programmatic-method-test
  ;; I guess this is okay output. Not great, but it is ok for now.
  (t/is (= "* `:default`,   "
           (#'method-table.common/describe-method :default nil))))
