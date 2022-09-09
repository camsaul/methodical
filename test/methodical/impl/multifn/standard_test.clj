(ns methodical.impl.multifn.standard-test
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.test :as t]
   [methodical.core :as m]
   [methodical.impl.multifn.standard :as multifn.standard]))

(set! *warn-on-reflection* true)

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
                 (multifn.standard/sort-dispatch-values dispatcher permutation)))))))

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
    (t/testing (pr-str dispatch-values)
      (t/is (= expected
               (multifn.standard/composite-effective-dispatch-value
                (m/multi-default-dispatcher (fn [x y] [x y]) :default-value ::default)
                [String ::parakeet]
                dispatch-values)))))
  (t/testing "If there's ambiguity between values, always prefer values from the first dispatch value"
    (t/is (= [String ::parakeet]
             (multifn.standard/composite-effective-dispatch-value
              (m/multi-default-dispatcher (fn [x y] [x y]) :default-value ::default)
              [String ::budgie]
              [[String ::parrot] [Number ::parrot] [Object ::parakeet]])))))

(derive ::d.can    ::d.thing)
(derive ::d.bird   ::d.thing)
(derive ::d.toucan ::d.can)
(derive ::d.toucan ::d.bird)

(t/deftest diamond-inheritance-effective-dispatch-value-test
  (t/testing "effective-dispatch-value with diamond inheritance should be calculated correctly (#91)"
    (let [mf (-> (m/default-multifn
                  (fn [x _m]
                    (keyword x)))
                 (m/add-primary-method ::d.thing (constantly nil))
                 (m/add-primary-method ::d.can (constantly nil))
                 (m/add-aux-method :before ::d.bird (constantly nil))
                 (m/add-aux-method :after ::d.can (constantly nil)))]
      (t/is (= ::d.toucan
               (m/effective-dispatch-value mf ::d.toucan))))
    (t/testing "Composite dispatch values"
      (let [mf (-> (m/default-multifn
                    (fn [x _m]
                      (keyword x)))
                   (m/add-primary-method [::d.thing ::d.thing] (constantly nil))
                   (m/add-primary-method [::d.can ::d.can] (constantly nil))
                   (m/add-aux-method :before [::d.bird ::d.bird] (constantly nil))
                   (m/add-aux-method :after [::d.can ::d.can] (constantly nil)))]
        (t/is (= [::d.toucan ::d.toucan]
                 (m/effective-dispatch-value mf [::d.toucan ::d.toucan])))))))

(t/deftest effective-dispatch-value-with-preferred-aux-methods-test
  (t/testing "effective-dispatch-value should ignore preferences (#104)"
    (let [h (-> (make-hierarchy)
                (derive :toucan :bird)
                (derive :toucan :can)
                (derive :parrot :bird))]
      (doseq [invoke-order [[:parrot :toucan]
                            [:toucan :parrot]]
              prefs        [nil
                            [:bird :can]
                            [:can :bird]]]
        (t/testing (str (format "\nInvoke %s before %s" (first invoke-order) (second invoke-order))
                        (when prefs
                          (format "\nprefer %s over %s" (first prefs) (second prefs))))
          (let [mf (-> (m/default-multifn :k, :hierarchy (atom h))
                       ;; apply `:bird` methods before `:can` methods
                       (m/add-aux-method :before :bird (fn [m] (update m :calls conj :before/bird)))
                       (m/add-aux-method :before :can (fn [m] (update m :calls conj :before/can)))
                       (m/add-primary-method :default (fn [_next-method m] (update m :calls conj :primary/default)))
                       (m/add-aux-method :around :default (fn [next-method m] (update (next-method m) :calls conj :around/default))))
                mf (if prefs
                     (m/prefer-method mf (first prefs) (second prefs))
                     mf)]
            (doseq [bird invoke-order]
              (t/testing (format "\nInvoke %s" bird)
                (t/testing "\neffective dispatch value"
                  (let [expected (case bird
                                   :parrot :bird
                                   :toucan :toucan)]
                    (t/testing (format "\ncalling mf for %s should use the method for %s" bird expected)
                      (t/is (= expected
                               (m/effective-dispatch-value mf bird))))))
                (t/is (= (case bird
                           :toucan {:k bird, :calls (case prefs
                                                      ;; if there is no preference between aux methods the order is
                                                      ;; indeterminant -- see #103 -- but for whatever reason this
                                                      ;; seems to be what happens for now
                                                      (nil [:bird :can])
                                                      [:before/bird :before/can :primary/default :around/default]

                                                      [:can :bird]
                                                      [:before/can :before/bird :primary/default :around/default])}
                           :parrot {:k bird, :calls [:before/bird :primary/default :around/default]})
                         (mf {:k bird, :calls []})))))))))))

(t/deftest consider-all-primary-methods-when-calculating-effective-dispatch-value-test
  (t/testing "effective-dispatch-value needs to consider *all* primary methods."
    (doseq [ks   [[:bird :toucan]
                  [:toucan :bird]]
            :let [h (-> (make-hierarchy)
                        (derive :bird :thing)
                        (derive :can :thing)
                        (derive :toucan :bird)
                        (derive :toucan :can))
                  m (-> (m/default-multifn :k, :hierarchy (atom h))
                        (m/add-primary-method :bird (fn [next-method m]
                                                      (cond-> (update m :calls conj :bird)
                                                        next-method
                                                        next-method)))
                        (m/add-primary-method :can (fn [next-method m]
                                                     (cond-> (update m :calls conj :can)
                                                       next-method
                                                       next-method)))
                        (m/prefer-method :bird :can))]
            k    ks]
      (t/testing (format "order = %s, testing %s" (pr-str ks) k)
        (t/testing `multifn.standard/composite-effective-dispatch-value
          (t/is (= k
                   (multifn.standard/composite-effective-dispatch-value m k [:bird :can]))))
        (t/testing `multifn.standard/effective-dispatch-value
          (t/is (= k
                   (multifn.standard/effective-dispatch-value
                    m
                    k
                    (m/matching-primary-methods m k)
                    (m/matching-aux-methods m k)))))
        (t/testing `multifn.standard/standard-effective-method
          (t/is (= k
                   (:dispatch-value (meta (multifn.standard/standard-effective-method m m m k))))))
        (t/testing `m/effective-dispatch-value
          (t/is (= k
                   (m/effective-dispatch-value m k))))
        (t/is (= (case k
                   :bird   {:k :bird, :calls [:bird]}
                   :toucan {:k :toucan, :calls [:bird :can]})
                 (m {:k k, :calls []})))))))

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
                   (meta (multifn.standard/standard-effective-method combo dispatcher method-table dv))))))))

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
                   (meta (multifn.standard/standard-effective-method combo dispatcher method-table dv)))))))))

(t/deftest nil-dispatch-values-test
  (t/testing "Dispatch values for `nil` should be calculated correctly (#112)"
    (doseq [order [[nil :default]
                   [:default nil]]]
      (t/testing (str "\norder = " (pr-str order))
        (let [mf* (atom nil)
              mf  (-> (m/multifn
                       (m/standard-multifn-impl
                        (m/thread-last-method-combination)
                        (m/multi-default-dispatcher identity)
                        (m/standard-method-table)))
                      (m/add-primary-method :default (fn [_next-method _m] :default))
                      (m/add-primary-method nil (fn [_next-method _m]
                                                  (@mf* :default)
                                                  nil)))]
          (reset! mf* mf)
          (doseq [x order]
            (t/testing (str \newline (pr-str x))
              (t/testing (str \newline `m/effective-dispatch-value)
                (t/is (= x
                         (m/effective-dispatch-value mf x))))
              (t/is (= x
                       (mf x))))))))))
