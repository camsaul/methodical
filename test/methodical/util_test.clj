(ns methodical.util-test
  (:require [clojure.test :as t]
            [methodical
             [core :as m]
             [impl :as impl]
             [interface :as i]
             [util :as u]]))

(t/deftest multifn?-test
  (t/is (= false
           (u/multifn? (Object.))))

  (t/is (= false
           (u/multifn? nil)))

  (t/is (= true
           (u/multifn? (m/default-multifn keyword)))))

(t/deftest primary-method-test
  (let [m1 (constantly [:char-sequence])
        m2 (constantly [:default])
        f  (-> (m/default-multifn class)
               (m/add-primary-method CharSequence m1)
               (m/add-primary-method :default m2))]
    (t/testing "primary-method"
      (t/is (= m1
               (u/primary-method f CharSequence))
            "primary-method should return primary methods with exactly the same dispatch value.")

      (t/is (= nil
               (u/primary-method f String))
            "`primary-method` should not return default or parent primary methods -- just the exact match."))

    (t/testing "applicable-primary-method"
      (t/is (= (m1 nil)
               ((u/applicable-primary-method f String) nil))
            "applicable-primary-method should give you the primary method that will be used for a dispatch value."))

    (t/testing "effective-primary-method"
      (let [f (m/add-aux-method f :before Object (fn [x] (conj x :before)))]
        (t/is (= [:char-sequence]
                 ((u/effective-primary-method f String) nil))
              "effective-primary-method should give you the combined effective primary method.")))))

;; aux-methods
(t/deftest aux-methods-test
  (let [m1 #(conj % :before-1)
        m2 #(conj % :before-2)
        m3 #(conj % :before-3)
        f  (-> (m/default-multifn class)
               (m/add-aux-method :before String m1)
               (m/add-aux-method :before String m2)
               (m/add-aux-method :before Object m3)
               (m/add-aux-method :after String m2)
               (m/add-aux-method :after Object m3))]
    (t/testing "aux-methods"
      (t/is (=
             {:before [m1 m2]}
             (u/aux-methods f :before String))
            "3-arity")

      (t/is (=
             {:before [m1 m2]
              :after  [m2]}
             (u/aux-methods f String))
            "2-ariy")

      (t/is (=
             {:before {String [m1 m2]
                       Object [m3]}
              :after {String [m2]
                      Object [m3]}}
             (u/aux-methods))
            "1-arity"))))

(t/deftest default-methods-test
  (let [m1 (constantly [:char-sequence])
        m2 (constantly [:default])
        f  (-> (m/default-multifn class)
               (m/add-primary-method CharSequence m1)
               (m/add-primary-method :default m2))]
    (t/testing "default-primary-method"
      (t/is (= m2
               (u/default-primary-method f))
            "should be able to get the default primary method"))

    (t/testing "default-aux-methods"
      (let [f' (-> f
                   (m/add-aux-method :before CharSequence 'm3)
                   (m/add-aux-method :before :default 'm4)
                   (m/add-aux-method :before :default 'm5))]
        (t/is (= {:before ['m4 'm5]}
                 (u/default-aux-methods f')))))

    (t/testing "default-effective-method"
      (t/is (= [:default]
               ((u/default-effective-method f) nil))))))

(t/deftest effective-dispatch-value-test
  (doseq [default-method? [true false]]
    (t/testing (format "default method? %s" default-method?)
      (let [f1 (cond-> (m/default-multifn class)
                 default-method? (m/add-primary-method :default (fn [_])))]
        (t/is (= (when default-method? :default)
                 (u/effective-dispatch-value f1 Object)
                 (u/effective-dispatch-value f1 nil)))
        (let [f2 (m/add-primary-method f1 Object (fn [_]))]
          (t/is (= Object
                   (u/effective-dispatch-value f2 Object)
                   (u/effective-dispatch-value f2 Integer)))
          (t/is (= (when default-method? :default)
                   (u/effective-dispatch-value f2 nil)))
          (let [f3 (-> f2
                       (m/add-aux-method :before Number (fn [_]))
                       (m/add-aux-method :around CharSequence (fn [_]))
                       (m/add-primary-method String (fn [_])))]
            (t/is (= Object
                     (u/effective-dispatch-value f3 java.util.Map)))
            (t/testing "primary method is more specific than aux method(s)"
              (t/is (= String
                       (u/effective-dispatch-value f3 String))))
            (t/testing "aux method(s) are more specific than primary method"
              (t/is (= Number
                       (u/effective-dispatch-value f3 Number)
                       (u/effective-dispatch-value f3 Integer)))))))))
  (t/testing "keyword aux methods"
    (derive ::parrot ::bird)
    (derive ::parakeet ::parrot)
    (let [f (-> (m/default-multifn :type)
                (m/add-primary-method :default (fn [_]))
                (m/add-aux-method :after ::bird (fn [_]))
                (m/add-aux-method :after ::parrot (fn [_])))]
      (doseq [[dv expected] {::dog :default
                             ::bird ::bird
                             ::parrot ::parrot
                             ::parakeet ::parrot}]
        (t/testing dv
          (t/is (= expected
                   (m/effective-dispatch-value f dv))))))))

(t/deftest dispatch-fn-test
  (t/testing "dispatch-fn"
    (let [f (m/default-multifn keyword)]
      (t/is (= :wow
               ((u/dispatch-fn f) "wow"))
            "dispatch-fn should return a function that can be used to get the dispatch value of arg(s)"))))

(t/deftest primary-methods-test
  (let [m1 (constantly [:char-sequence])
        m2 (constantly [:default])
        f  (-> (m/default-multifn class)
               (m/add-primary-method CharSequence m1)
               (m/add-primary-method :default m2))]
    (t/testing "remove-all-primary-methods"
      (t/is (= nil
               (seq (i/primary-methods (u/remove-all-primary-methods f))))))

    (t/testing "add-primary-method!"
      (def ^:private add-primary-method-multifn (m/default-multifn keyword))
      (t/testing "adding a *new* primary method"
        (u/add-primary-method! #'add-primary-method-multifn ::key (constantly ::value))
        (t/is (= ::value
                 (add-primary-method-multifn ::key))))

      (t/testing "replacing a primary method"
        (u/add-primary-method! #'add-primary-method-multifn ::key (constantly ::value-2))
        (t/is (= ::value-2
                 (add-primary-method-multifn ::key)))))

    (t/testing "remove-primary-method!"
      (def ^:private remove-primary-method-multifn f)
      (t/is (= [:char-sequence]
               (remove-primary-method-multifn "String")))

      (u/remove-primary-method! #'remove-primary-method-multifn CharSequence)
      (t/is (= [:default]
               (remove-primary-method-multifn "String"))))

    (t/testing "remove-all-primary-methods!"
      (def ^:private remove-all-primary-methods f)
      (u/remove-all-primary-methods! #'remove-primary-method-multifn)
      (t/is (= nil
               (seq (i/primary-methods remove-primary-method-multifn)))))))

(t/deftest aux-methods-test
  (let [f (-> (m/default-multifn class)
              (m/add-aux-method :before String 'm1)
              (m/add-aux-method :before Object 'm2)
              (m/add-aux-method :after String 'm2)
              (m/add-aux-method :after String 'm3)
              (m/add-aux-method :after Object 'm2)
              (m/add-aux-method :after Object 'm4))]

    (t/testing "remove-all-aux-methods"
      (t/is (= nil
               (seq (m/aux-methods (u/remove-all-aux-methods f))))))

    (t/testing "remove-all-aux-methods-for-dispatch-val"
      (t/is (= {:before {Object ['m2]}
                :after  {Object ['m2 'm4]}}
               (m/aux-methods (u/remove-all-aux-methods-for-dispatch-val f String)))))
    ;; TODO

    (t/testing "remove-all-aux-methods!"
      (def ^:private remove-all-aux-methods-multifn f)
      (u/remove-all-aux-methods! #'remove-all-aux-methods-multifn)
      (t/is (= nil
               (seq (m/aux-methods remove-all-aux-methods-multifn)))))

    (t/testing "add-aux-method!"
      (def ^:private add-aux-method-multifn f)
      (u/add-aux-method! #'add-aux-method-multifn :around String 'm1)
      (t/is (= {:before {String ['m1]
                         Object ['m2]}
                :after  {String ['m2 'm3]
                         Object ['m2 'm4]}
                :around {String ['m1]}}
               (m/aux-methods add-aux-method-multifn))))

    (t/testing "remove-aux-method!"
      (def ^:private remove-aux-method-multifn f)
      (u/remove-aux-method! #'remove-aux-method-multifn :after String 'm2)
      (t/is (= {:before {String ['m1]
                         Object ['m2]}
                :after  {String ['m3]
                         Object ['m2 'm4]}}
               (m/aux-methods remove-aux-method-multifn)))

      (u/remove-aux-method! #'remove-aux-method-multifn :before String 'm1)
      (t/is (= {:before {Object ['m2]}
                :after  {String ['m3]
                         Object ['m2 'm4]}}
               (m/aux-methods remove-aux-method-multifn))
            "Removing the last method for the dispatch value should remove that dispatch value entirely."))

    (t/testing "remove-all-aux-methods-for-dispatch-val!"
      (def ^:private remove-all-aux-methods-for-dispatch-val-multifn f)
      (u/remove-all-aux-methods-for-dispatch-val! #'remove-all-aux-methods-for-dispatch-val-multifn String)
      (t/is (= {:before {Object ['m2]}
                :after  {Object ['m2 'm4]}}
               (m/aux-methods remove-all-aux-methods-for-dispatch-val-multifn))))

    (t/testing "matching-aux-methods"
      (t/is (= {:before '[m1 m2]
                :after  '[m2 m3 m2 m4]}
               (u/matching-aux-methods f String)
               (u/matching-aux-methods f f String))))))

(t/deftest aux-methods-unique-key-test
  (t/testing "non-destructive operations")
  (let [multifn (u/add-aux-method-with-unique-key (impl/default-multifn keyword) :before ::parent
                                                  #(conj % :before) "my unique key")]
    (t/testing "adding a method with a unique key"
      (t/is (= 1
               (count (u/aux-methods multifn :before ::parent)))
            "Should have 1 method after calling `add-aux-method-with-unique-key`")

      (t/is (= [:before]
               ((first (u/aux-methods multifn :before ::parent)) []))))

    (t/testing "calling `add-aux-method-with-unique-key` with the same key should replace the original method"
      (let [multifn' (u/add-aux-method-with-unique-key multifn :before ::parent #(conj % :before-2) "my unique key")]
        (t/is (= 1
                 (count (u/aux-methods multifn' :before ::parent))))

        (t/is (= [:before-2]
                 ((first (u/aux-methods multifn' :before ::parent)) [])))))

    (t/testing "`remove-aux-method-with-unique-key``"
      (let [multifn' (u/remove-aux-method-with-unique-key multifn :before ::parent "my unique key")]
        (t/is (= 0
                 (count (u/aux-methods multifn' :before ::parent)))))))

  (t/testing "destructive operations"
    (def ^:private unique-key-multifn nil)
    (m/defmulti ^:private unique-key-multifn keyword)
    (assert (some? unique-key-multifn))

    (u/add-aux-method-with-unique-key! #'unique-key-multifn :before ::key #(conj % :before) "Florida key")
    (t/is (= 1
             (count (u/aux-methods unique-key-multifn :before ::key)))
          "should be able to desctructively add an aux method with a unique key")

    (u/remove-aux-method-with-unique-key! #'unique-key-multifn :before ::key "Florida key")
    (t/is (= 0
             (count (u/aux-methods unique-key-multifn :before ::key)))
          "should be able to desctructively remove an aux method with a unique key")))

(t/deftest remove-all-methods-test
  (let [f (-> (m/default-multifn class)
              (m/add-aux-method :before String 'm1)
              (m/add-aux-method :after String 'm2)
              (m/add-aux-method :around Object 'm3)
              (m/add-primary-method String 'p1)
              (m/add-primary-method Object 'p2))]
    (t/testing "remove-all-methods"
      (let [f' (u/remove-all-methods f)]
        (t/is (= nil
                 (seq (i/primary-methods f'))))

        (t/is (= nil
                 (seq (i/aux-methods f'))))))

    (t/testing "remove-all-methods!"
      (def ^:private remove-all-methods-multifn f)
      (u/remove-all-methods! #'remove-all-methods-multifn)
      (t/is (= nil
               (seq (i/primary-methods remove-all-methods-multifn))))

      (t/is (= nil
               (seq (i/aux-methods remove-all-methods-multifn)))))))
