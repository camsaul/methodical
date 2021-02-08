(ns methodical.impl.dispatcher.multi-default-test
  (:require [clojure.test :as t]
            [methodical [core :as m] interface]
            [methodical.impl.dispatcher.multi-default :as multi-default])
  (:import methodical.interface.MethodTable))

(t/deftest partially-specialized-default-dispatch-values-test
  (doseq [x       [:x ::x nil]
          y       [:y ::y nil]
          z       [:z ::z nil]
          default [:default ::default :a]]
    (t/testing (format "default value = %s dispatch-value = %s" default [x y z])
      (t/is (= [[x       y       default]
                [x       default z]
                [x       default default]
                [default y       z]
                [default y       default]
                [default default z]
                [default default default]]
               (multi-default/partially-specialized-default-dispatch-values [x y z] default))))))

(t/deftest partially-specialized-default-dispatch-values-test
  (doseq [default [:default ::default :a]]
    (doseq [x [:x nil]]
      (t/testing (format "dispatch value = %s" (pr-str x))
        (t/is (= nil
                 (multi-default/partially-specialized-default-dispatch-values x default))
              "If the dispatch value isn't sequential, we shouldn't calculate partial default dispatch values")))
    (doseq [x [:x nil]
            y [:y nil]]
      (t/testing (format "default value = %s dispatch-value = %s" default [x y])
        (t/is (= [[x       default]
                  [default y]
                  [default default]]
                 (multi-default/partially-specialized-default-dispatch-values [x y] default))
              (str "If the dispatch value is sequental, but default is not, we should return a sequence of partial"
                   " default dispatch values"))
        (t/is (= nil
                 (multi-default/partially-specialized-default-dispatch-values [x y] [default x]))
              "If the default value is sequential, we shouldn't calculate partial default dispatch values")))))

(t/deftest matching-primary-methods-test
  ;; Conisder what the correct behavior for `:letter` should be
  (doseq [default                           [:default ::default nil #_:letter]
          [dispatch-value expected-methods] (merge
                                             {[:X :Y]           '[XY Xy xY xy Xd xd dY dy dd d]
                                              [:x :y]           '[xy xd dy dd d]
                                              [:X :y]           '[Xy xy Xd xd dy dd d]
                                              [:x :Y]           '[xY xy xd dY dy dd d]
                                              [:X nil]          '[Xd xd dd d]
                                              [:x nil]          '[xd dd d]
                                              [nil :Y]          '[dY dy dd d]
                                              [nil :y]          '[dy dd d]
                                              [default default] '[dd d]
                                              default           '[d]}
                                             ;; these are merged seperately in in case default *is* nil
                                             {[nil nil] '[dd d]
                                              nil       '[d]}
                                             {[default nil] '[dd d]}
                                             {[nil default] '[dd d]})
          :let                              [table (reify MethodTable
                                                     (primary-methods [_]
                                                       {[:X :Y]           'XY
                                                        [:X :y]           'Xy
                                                        [:x :Y]           'xY
                                                        [:x :y]           'xy
                                                        [:X default]      'Xd
                                                        [:x default]      'xd
                                                        [default :Y]      'dY
                                                        [default :y]      'dy
                                                        [default default] 'dd
                                                        default           'd}))
                                             h     (-> (make-hierarchy)
                                                       (derive :X :x)
                                                       (derive :Y :y)
                                                       (derive :x :letter)
                                                       (derive :y :letter))]]
    (t/testing (format "default value = %s dispatch value = %s" default (pr-str dispatch-value))
      (t/is (= expected-methods
               (multi-default/matching-primary-methods
                {:hierarchy                h
                 :prefs                    nil
                 :default-value            default
                 :method-table             table
                 :dispatch-value           dispatch-value
                 :unambiguous-pairs-seq-fn (fn [& args] (last args))}))))))

(t/deftest ambiguity-test
  (doseq [default [:default :thing nil]]
    (t/testing (format "default value = %s" default)
      (let [table (reify MethodTable
                    (primary-methods [_]
                      {[:large-beak :shape]  (constantly :shape)
                       [:large-beak default] (constantly :large-beak)
                       [:eats-fruit default] (constantly :eats-fruit)
                       [default default]     (constantly default)}))
            h     (-> (make-hierarchy)
                      (derive :toucan :large-beak)
                      (derive :toucan :eats-fruit)
                      (derive :square :shape)
                      (derive :circle :shape)
                      (derive :shape :thing)
                      (derive :large-beak :thing)
                      (derive :eats-fruit :thing))]
        (letfn [(invoke-with-prefs [prefs dispatch-val]
                  (let [[matching-method] (multi-default/matching-primary-methods
                                           {:hierarchy      h
                                            :prefs          prefs
                                            :default-value  default
                                            :method-table   table
                                            :dispatch-value dispatch-val})]
                    (assert matching-method)
                    (matching-method dispatch-val)))]
          (t/testing (str "If two partially-specialized default methods are ambiguous, we should throw an Exception the"
                          " same we would for other methods")
            (t/is (thrown-with-msg?
                   IllegalArgumentException
                   (re-pattern (str "Multiple methods match dispatch value: \\[:toucan 100] ->"
                                    (format " \\[:large-beak %s\\] and \\[:eats-fruit %s\\], and neither is preferred."
                                            (pr-str default) (pr-str default))))
                   (invoke-with-prefs nil [:toucan 100]))))
          (t/testing "Even if some default methods are ambiguous we should be able to use unambiguous non-defaults"
            (t/is (= :shape
                     (invoke-with-prefs nil [:toucan :circle]))))
          (t/testing "Should be able to define prefs to get around ambiguity"
            (let [prefs {[:large-beak default] #{[:eats-fruit default]}}]
              (t/is (= :large-beak
                       (invoke-with-prefs prefs [:toucan 100])
                       (invoke-with-prefs prefs [:large-beak 100]))))))))))

(t/deftest matching-aux-methods-test
  (doseq [method-type                       [:before :after :around]
          default                           [:default #_:letter nil]
          [dispatch-value expected-methods] (merge
                                             {[:X :Y]           '[XY Xy xY xy Xd xd dY dy dd d]
                                              [:x :y]           '[xy xd dy dd d]
                                              [:X :y]           '[Xy xy Xd xd dy dd d]
                                              [:x :Y]           '[xY xy xd dY dy dd d]
                                              [:X nil]          '[Xd xd dd d]
                                              [:x nil]          '[xd dd d]
                                              [nil :Y]          '[dY dy dd d]
                                              [nil :y]          '[dy dd d]
                                              [default default] '[dd d]
                                              default           '[d]}
                                             {[nil nil] '[dd d]
                                              nil       '[d]}
                                             {[default nil] '[dd d]}
                                             {[nil default] '[dd d]})
          :let                              [table (reify MethodTable
                                                     (aux-methods [_]
                                                       {method-type {[:X :Y]           ['XY]
                                                                     [:X :y]           ['Xy]
                                                                     [:x :Y]           ['xY]
                                                                     [:x :y]           ['xy]
                                                                     [:X default]      ['Xd]
                                                                     [:x default]      ['xd]
                                                                     [default :Y]      ['dY]
                                                                     [default :y]      ['dy]
                                                                     [default default] ['dd]
                                                                     default           ['d]}}))
                                             h     (-> (make-hierarchy)
                                                       (derive :X :x)
                                                       (derive :Y :y)
                                                       (derive :x :letter)
                                                       (derive :y :letter))]]
    (t/testing (format "method-type = %s default value = %s dispatch value = %s"
                       method-type default (pr-str dispatch-value))
      (t/is (= {method-type expected-methods}
               (multi-default/matching-aux-methods
                {:hierarchy      h
                 :prefs          nil #_prefs
                 :default-value  default
                 :method-table   table
                 :dispatch-value dispatch-value}))))))

(def ^:private hierarchy (-> (make-hierarchy)
                             (derive :X :x)))

;; both of these multifns should be equivalent, now that `multi-default-dispatcher` is the new default
(m/defmulti ^:private multifn-with-dispatcher
  :dispatcher (m/multi-default-dispatcher (fn [x y] [(keyword x) (class y)])
                :hierarchy #'hierarchy))

(m/defmulti ^:private default-multifn
  (fn [x y] [(keyword x) (class y)])
  :hierarchy #'hierarchy)

(m/defmethod multifn-with-dispatcher [:x String]         [_ _] 'xS)
(m/defmethod multifn-with-dispatcher [:x Object]         [_ _] 'xO)
(m/defmethod multifn-with-dispatcher [:X :default]       [_ _] 'Xd)
(m/defmethod multifn-with-dispatcher [:x :default]       [_ _] 'xd)
(m/defmethod multifn-with-dispatcher [:default String]   [_ _] 'dS)
(m/defmethod multifn-with-dispatcher [:default Object]   [_ _] 'dO)
(m/defmethod multifn-with-dispatcher [:default :default] [_ _] 'dd)
(m/defmethod multifn-with-dispatcher :default            [_ _] 'd)

(m/defmethod default-multifn [:x String]         [_ _] 'xS)
(m/defmethod default-multifn [:x Object]         [_ _] 'xO)
(m/defmethod default-multifn [:X :default]       [_ _] 'Xd)
(m/defmethod default-multifn [:x :default]       [_ _] 'xd)
(m/defmethod default-multifn [:default String]   [_ _] 'dS)
(m/defmethod default-multifn [:default Object]   [_ _] 'dO)
(m/defmethod default-multifn [:default :default] [_ _] 'dd)
(m/defmethod default-multifn :default            [_ _] 'd)

(t/deftest e2e-test
  (doseq [[m-symb m] {'multifn-with-dispatcher multifn-with-dispatcher
                      'default-multifn         default-multifn}]
    (t/testing m-symb
      (t/testing "[specialized specialized]"
        (t/is (= 'xS
                 (m :X "Wow!")
                 (m :x "Wow!")))
        (t/is (= 'xO
                 (m :X 123)
                 (m :x 123))))
      (t/testing "[specialized :default]"
        (t/is (= 'Xd
                 (m :X nil)))
        (t/is (= 'xd
                 (m :x nil))))
      (t/testing "[:default specialized]"
        (t/is (= 'dS
                 (m :Y "Wow!")
                 (m nil "Wow!")))
        (t/is (= 'dO
                 (m :Y 123)
                 (m nil 123))))
      (t/testing "[:default :default]"
        (t/is (= 'dd
                 (m :Y nil)
                 (m nil nil)))))))
