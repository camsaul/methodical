(ns methodical.util-test
  (:require [clojure.test :refer :all]
            [methodical
             [core :as m]
             [impl :as impl]
             [interface :as i]
             [util :as u]]))

(deftest multifn?-test
  (is (= false
         (u/multifn? (Object.))))

  (is (= false
         (u/multifn? nil)))

  (is (= true
         (u/multifn? (m/default-multifn keyword)))))

(deftest primary-method-test
  (let [m1 (constantly [:char-sequence])
        m2 (constantly [:default])
        f  (-> (m/default-multifn class)
               (m/add-primary-method CharSequence m1)
               (m/add-primary-method :default m2))]
    (testing "primary-method"
      (is (= m1
             (u/primary-method f CharSequence))
          "primary-method should return primary methods with exactly the same dispatch value.")

      (is (= nil
             (u/primary-method f String))
          "`primary-method` should not return default or parent primary methods -- just the exact match."))

    (testing "applicable-primary-method"
      (is (= m1
             (u/applicable-primary-method f String))
          "applicable-primary-method should give you the primary method that will be used for a dispatch value."))

    (testing "effective-primary-method"
      (let [f (m/add-aux-method f :before Object (fn [x] (conj x :before)))]
        (is (= [:char-sequence]
               ((u/effective-primary-method f String) nil))
            "effective-primary-method should give you the combined effective primary method.")))))

;; aux-methods
(deftest aux-methods-test
  (let [m1 #(conj % :before-1)
        m2 #(conj % :before-2)
        m3 #(conj % :before-3)
        f  (-> (m/default-multifn class)
               (m/add-aux-method :before String m1)
               (m/add-aux-method :before String m2)
               (m/add-aux-method :before Object m3)
               (m/add-aux-method :after String m2)
               (m/add-aux-method :after Object m3))]
    (testing "aux-methods"
      (is (=
           {:before [m1 m2]}
           (u/aux-methods f :before String))
          "3-arity")

      (is (=
           {:before [m1 m2]
            :after  [m2]}
           (u/aux-methods f String))
          "2-ariy")

      (is (=
           {:before {String [m1 m2]
                     Object [m3]}
            :after {String [m2]
                    Object [m3]}}
           (u/aux-methods))
          "1-arity"))))

(deftest default-methods-test
  (let [m1 (constantly [:char-sequence])
        m2 (constantly [:default])
        f  (-> (m/default-multifn class)
               (m/add-primary-method CharSequence m1)
               (m/add-primary-method :default m2))]
    (testing "default-primary-method"
      (is (= m2
             (u/default-primary-method f))
          "should be able to get the default primary method"))

    (testing "default-aux-methods"
      (let [f' (-> f
                   (m/add-aux-method :before CharSequence 'm3)
                   (m/add-aux-method :before :default 'm4)
                   (m/add-aux-method :before :default 'm5))]
        (is (= {:before ['m4 'm5]}
               (u/default-aux-methods f')))))

    (testing "default-effective-method")
    (is (= [:default]
           ((u/default-effective-method f) nil)))))

(deftest dispatch-fn-test
  (testing "dispatch-fn"
    (let [f (m/default-multifn keyword)]
      (is (= :wow
             ((u/dispatch-fn f) "wow"))
          "dispatch-fn should return a function that can be used to get the dispatch value of arg(s)"))))

(deftest primary-methods-test
  (let [m1 (constantly [:char-sequence])
        m2 (constantly [:default])
        f  (-> (m/default-multifn class)
               (m/add-primary-method CharSequence m1)
               (m/add-primary-method :default m2))]
    (testing "remove-all-primary-methods"
      (is (= nil
             (seq (i/primary-methods (u/remove-all-primary-methods f))))))

    (testing "add-primary-method!"
      (def ^:private add-primary-method-multifn (m/default-multifn keyword))
      (testing "adding a *new* primary method"
        (u/add-primary-method! #'add-primary-method-multifn ::key (constantly ::value))
        (is (= ::value
               (add-primary-method-multifn ::key))))

      (testing "replacing a primary method"
        (u/add-primary-method! #'add-primary-method-multifn ::key (constantly ::value-2))
        (is (= ::value-2
               (add-primary-method-multifn ::key)))))

    (testing "remove-primary-method!"
      (def ^:private remove-primary-method-multifn f)
      (is (= [:char-sequence]
             (remove-primary-method-multifn "String")))

      (u/remove-primary-method! #'remove-primary-method-multifn CharSequence)
      (is (= [:default]
             (remove-primary-method-multifn "String"))))

    (testing "remove-all-primary-methods!"
      (def ^:private remove-all-primary-methods f)
      (u/remove-all-primary-methods! #'remove-primary-method-multifn)
      (is (= nil
             (seq (i/primary-methods remove-primary-method-multifn)))))))

(deftest aux-methods-test
  (let [f (-> (m/default-multifn class)
              (m/add-aux-method :before String 'm1)
              (m/add-aux-method :before Object 'm2)
              (m/add-aux-method :after String 'm2)
              (m/add-aux-method :after String 'm3)
              (m/add-aux-method :after Object 'm2)
              (m/add-aux-method :after Object 'm4))]

    (testing "remove-all-aux-methods"
      (is (= nil
             (seq (m/aux-methods (u/remove-all-aux-methods f))))))

    (testing "remove-all-aux-methods-for-dispatch-val"
      (is (= {:before {Object ['m2]}
              :after  {Object ['m2 'm4]}}
             (m/aux-methods (u/remove-all-aux-methods-for-dispatch-val f String)))))
    ;; TODO

    (testing "remove-all-aux-methods!"
      (def ^:private remove-all-aux-methods-multifn f)
      (u/remove-all-aux-methods! #'remove-all-aux-methods-multifn)
      (is (= nil
             (seq (m/aux-methods remove-all-aux-methods-multifn)))))

    (testing "add-aux-method!"
      (def ^:private add-aux-method-multifn f)
      (u/add-aux-method! #'add-aux-method-multifn :around String 'm1)
      (is (= {:before {String ['m1]
                       Object ['m2]}
              :after  {String ['m2 'm3]
                       Object ['m2 'm4]}
              :around {String ['m1]}}
             (m/aux-methods add-aux-method-multifn))))

    (testing "remove-aux-method!"
      (def ^:private remove-aux-method-multifn f)
      (u/remove-aux-method! #'remove-aux-method-multifn :after String 'm2)
      (is (= {:before {String ['m1]
                       Object ['m2]}
              :after  {String ['m3]
                       Object ['m2 'm4]}}
             (m/aux-methods remove-aux-method-multifn)))

      (u/remove-aux-method! #'remove-aux-method-multifn :before String 'm1)
      (is (= {:before {Object ['m2]}
              :after  {String ['m3]
                       Object ['m2 'm4]}}
             (m/aux-methods remove-aux-method-multifn))
          "Removing the last method for the dispatch value should remove that dispatch value entirely."))

    (testing "remove-all-aux-methods-for-dispatch-val!"
      (def ^:private remove-all-aux-methods-for-dispatch-val-multifn f)
      (u/remove-all-aux-methods-for-dispatch-val! #'remove-all-aux-methods-for-dispatch-val-multifn String)
      (is (= {:before {Object ['m2]}
              :after  {Object ['m2 'm4]}}
             (m/aux-methods remove-all-aux-methods-for-dispatch-val-multifn))))))

(deftest aux-methods-unique-key-test
  (testing "non-destructive operations")
  (let [multifn (u/add-aux-method-with-unique-key (impl/default-multifn keyword) :before ::parent
                                                  #(conj % :before) "my unique key")]
    (testing "adding a method with a unique key"
      (is (= 1
             (count (u/aux-methods multifn :before ::parent)))
          "Should have 1 method after calling `add-aux-method-with-unique-key`")

      (is (= [:before]
             ((first (u/aux-methods multifn :before ::parent)) []))))

    (testing "calling `add-aux-method-with-unique-key` with the same key should replace the original method"
      (let [multifn' (u/add-aux-method-with-unique-key multifn :before ::parent #(conj % :before-2) "my unique key")]
        (is (= 1
               (count (u/aux-methods multifn' :before ::parent))))

        (is (= [:before-2]
               ((first (u/aux-methods multifn' :before ::parent)) [])))))

    (testing "`remove-aux-method-with-unique-key``"
      (let [multifn' (u/remove-aux-method-with-unique-key multifn :before ::parent "my unique key")]
        (is (= 0
               (count (u/aux-methods multifn' :before ::parent)))))))

  (testing "destructive operations"
    (def ^:private unique-key-multifn nil)
    (m/defmulti ^:private unique-key-multifn keyword)
    (assert (some? unique-key-multifn))

    (u/add-aux-method-with-unique-key! #'unique-key-multifn :before ::key #(conj % :before) "Florida key")
    (is (= 1
           (count (u/aux-methods unique-key-multifn :before ::key)))
        "should be able to desctructively add an aux method with a unique key")

    (u/remove-aux-method-with-unique-key! #'unique-key-multifn :before ::key "Florida key")
    (is (= 0
           (count (u/aux-methods unique-key-multifn :before ::key)))
        "should be able to desctructively remove an aux method with a unique key")))

(deftest remove-all-methods-test
  (let [f (-> (m/default-multifn class)
              (m/add-aux-method :before String 'm1)
              (m/add-aux-method :after String 'm2)
              (m/add-aux-method :around Object 'm3)
              (m/add-primary-method String 'p1)
              (m/add-primary-method Object 'p2))]
    (testing "remove-all-methods"
      (let [f' (u/remove-all-methods f)]
        (is (= nil
               (seq (i/primary-methods f'))))

        (is (= nil
               (seq (i/aux-methods f'))))))

    (testing "remove-all-methods!"
      (def ^:private remove-all-methods-multifn f)
      (u/remove-all-methods! #'remove-all-methods-multifn)
      (is (= nil
             (seq (i/primary-methods remove-all-methods-multifn))))

      (is (= nil
             (seq (i/aux-methods remove-all-methods-multifn)))))))
