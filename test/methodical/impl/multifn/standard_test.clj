(ns methodical.impl.multifn.standard-test
  (:require [clojure.math.combinatorics :as combo]
            [clojure.test :as t]
            [methodical.core :as m]
            [methodical.impl.multifn.standard :as standard]))

(derive ::parrot ::bird)
(derive ::parakeet ::parrot)
(derive ::budgie ::parakeet)
(derive ::love-bird ::parrot)

(t/deftest sort-dispatch-values-test
  (let [dispatcher (m/multi-default-dispatcher (fn [x y] [x y]) :default-value ::default)]
    ;; in cases where there's ambiguity (e.g. `[Integer ::parrot]` and `[Double ::parrot]`, keep the first value
    (doseq [permutation (combo/permutations [[Integer ::parrot] [Number ::parrot] [Object ::bird] ::default])
            :let        [permutation (cons [Double ::parrot] permutation)]]
      (t/testing (vec permutation)
        (t/is (= [[Double ::parrot] [Integer ::parrot] [Number ::parrot] [Object ::bird] ::default]
                 (standard/sort-dispatch-values dispatcher permutation)))))))

(t/deftest composite-effective-dispatch-value-test
  (doseq [[dispatch-values expected]
          {[[Object ::parrot]]
           [Object ::parrot]

           [[Object ::parrot] [String ::bird]]
           [String ::parrot]

           [[Object ::parrot] [String ::default]]
           [String ::parrot]

           [[Object ::parrot] [String ::default] ::default]
           [String ::parrot]

           [::default ::default]
           ::default}
          dispatch-values (distinct (combo/permutations dispatch-values))]
    (t/testing dispatch-values
      (t/is (= expected
               (standard/composite-effective-dispatch-value
                (m/multi-default-dispatcher (fn [x y] [x y]) :default-value ::default)
                dispatch-values)))))
  (t/testing "If there's ambiguity between values, always prefer values from the first dispatch value"
    (t/is (= [String ::parakeet]
             (standard/composite-effective-dispatch-value
              (m/multi-default-dispatcher (fn [x y] [x y]) :default-value ::default)
              [[String ::parrot] [Number ::parrot] [Object ::parakeet]])))))

(t/deftest standard-effective-method-dispatch-value-test
  (t/testing "standard-effective-method should return a method with the correct ^:dispatch-value metadata"
    (let [combo        (m/thread-last-method-combination)
          dispatcher   (m/multi-default-dispatcher :type)
          method-table (-> (m/standard-method-table)
                           (m/add-primary-method :default (fn [_]))
                           (m/add-primary-method ::parakeet (fn [_]))
                           (m/add-aux-method :after ::bird (fn [_]))
                           (m/add-aux-method :after ::parrot (fn [_])))]
      (doseq [[dv expected] {::dog       :default
                             ::bird      ::bird
                             ::parrot    ::parrot
                             ::parakeet  ::parakeet
                             ::budgie    ::parakeet
                             ::love-bird ::parrot}]
        (t/testing dv
          (t/is (= {:dispatch-value expected}
                   (meta (standard/standard-effective-method combo dispatcher method-table dv))))))))

  (t/testing "multiple dispatch values"
    (let [combo        (m/thread-last-method-combination)
          dispatcher   (m/multi-default-dispatcher vector)
          method-table (-> (m/standard-method-table)
                           (m/add-primary-method :default (fn [_]))
                           (m/add-aux-method :after [:default ::bird] (fn [_]))
                           (m/add-aux-method :after [:default ::parrot] (fn [_]))
                           (m/add-aux-method :before [Object :default] (fn [_]))
                           (m/add-aux-method :before [Number :default] (fn [_])))]
      (doseq [[dv1 expected-1] {nil     :default
                                Object  Object
                                String  Object
                                Number  Number
                                Integer Number}
              [dv2 expected-2] {::dog      :default
                                ::bird     ::bird
                                ::parrot   ::parrot
                                ::parakeet ::parrot}
              :let             [dv [dv1 dv2]]]
        (t/testing dv
          (t/is (= {:dispatch-value (if (= [expected-1 expected-2] [:default :default])
                                      :default
                                      [expected-1 expected-2])}
                   (meta (standard/standard-effective-method combo dispatcher method-table dv)))))))))
