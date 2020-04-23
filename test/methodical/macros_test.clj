(ns methodical.macros-test
  (:require [clojure.test :as t]
            [methodical
             [impl :as impl]
             [interface :as i]
             [macros :as m]
             [util :as u]]
            [potemkin.namespaces :as p.namespaces]))

(m/defmulti ^:private mf1 :type)

(m/define-primary-method mf1 :x
  [m]
  (assoc m :method :x))

(t/deftest macros-test
  (t/is (= mf1 (let [impl    (impl/standard-multifn-impl
                              (impl/thread-last-method-combination)
                              (impl/multi-default-dispatcher :type)
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

;; make a var that is basically an alias for another var. This mimics the way Potemkin import-vars works
(intern *ns* 'mf4 mf3)
(alter-meta! #'mf4 (constantly (meta #'mf3)))

(p.namespaces/link-vars #'mf3 #'mf4)

(m/defmethod mf4 :y
  [m]
  (assoc m :method :y))

(t/deftest defmethod-test
  (t/testing "We should be able to define new primary & aux methods using `defmethod`"
    (t/is (identical? mf3 mf4))
    (t/is (= (mf3 {:type :x})
             {:type :x, :after? true, :method :x})))

  (t/testing "`defmethod` should alter the actual var if the one it is called on is 'imported' (e.g. with Potemkin)"
    (t/is (= {:type :y, :method :y, :after? true}
             (mf3 {:type :y})
             (mf4 {:type :y})))))
