(ns methodical.macros-test
  (:require [clojure.test :as t]
            [methodical
             [impl :as impl]
             [interface :as i]
             [macros :as m]
             [util :as u]]))

(m/defmulti ^:private mf1 :type)

(m/define-primary-method mf1 :x
  [m]
  (assoc m :method :x))

(t/deftest macros-test
  (t/is (= mf1 (let [impl    (impl/standard-multifn-impl
                              (impl/thread-last-method-combination)
                              (impl/standard-dispatcher :type)
                              (impl/standard-method-table))
                     multifn (impl/multifn impl nil (impl/watching-cache
                                                     (impl/simple-cache)
                                                     [#'clojure.core/global-hierarchy]))]
                 (i/add-primary-method multifn :x (u/primary-method mf1 :x))))
        "A Methodical multifn defined via `defmulti` macros should be equivalent to one created using various `impl/`
      constructors.")
  (t/is (= (mf1 {:type :x})
           {:type :x, :method :x})
        "We should be able to define new primary methods using `define-primary-method`"))

(m/defmulti ^:private mf2 :type)

(m/define-primary-method mf2 :x
  [m]
  (assoc m :method :x))

(m/define-aux-method mf2 :before :default
  [m]
  (assoc m :before? true))

(t/deftest define-aux-method-test
  (t/is (= (mf2 {:type :x})
           {:type :x, :before? true, :method :x})
        "We should be able to define new aux methods using `define-aux-method`"))

(m/defmulti ^:private mf3 :type)

(m/defmethod mf3 :x
  [m]
  (assoc m :method :x))

(m/defmethod mf3 :after :default
  [m]
  (assoc m :after? true))

(t/deftest defmethod-test
  (t/is (= (mf3 {:type :x})
           {:type :x, :after? true, :method :x})
        "We should be able to define new primary & aux methods using `defmethod`"))
