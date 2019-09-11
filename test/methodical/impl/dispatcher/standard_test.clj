(ns methodical.impl.dispatcher.standard-test
  (:require [clojure.test :refer :all]
            [methodical
             [impl :as impl]
             [interface :as i]]))

(deftest equality-test
  (is (= (impl/standard-dispatcher keyword)
         (impl/standard-dispatcher keyword))
      "Two standard dispatchers with the same args should be considered equal.")
  (is (= false (= (impl/standard-dispatcher keyword)
                  (impl/standard-dispatcher keyword :default-value :different)))
      "Two standard dispatchers with different args should *not* be considered equal."))

(defn- method-table [primary-methods aux-methods]
  (reify i/MethodTable
    (primary-methods [_] primary-methods)
    (aux-methods [_] aux-methods)))

(def ^:private basic-hierarchy
  (-> (make-hierarchy)
      (derive :child :parent)
      (derive :parent :grandparent)))

(deftest matching-primary-methods-test
  (let [dispatcher (impl/standard-dispatcher keyword :hierarchy #'basic-hierarchy)]
    (testing "matching-primary-methods should return all matches in order of specificity."
      (let [method-table (method-table {:child 'child, :parent 'parent, :grandparent 'grandparent} nil)]
        (is (= '[child parent grandparent]
               (i/matching-primary-methods dispatcher method-table :child)))

        (is (= '[parent grandparent]
               (i/matching-primary-methods dispatcher method-table :parent)))))

    (testing "default primary methods"
      (let [method-table (method-table {:child       'child
                                        :parent      'parent
                                        :grandparent 'grandparent
                                        :default     'default} nil)]
        (is (= '[parent grandparent default]
               (i/matching-primary-methods dispatcher method-table :parent))
            "default methods should be included if they exist")

        (is (= '[default]
               (i/matching-primary-methods dispatcher method-table :cousin))
            "If there are otherwise no matches, default should be returned (but nothing else)")

        (testing "default methods should not be included twice if dispatch-value derives from it"
          (let [dispatcher-with-custom-default (impl/standard-dispatcher keyword
                                                 :hierarchy #'basic-hierarchy
                                                 :default-value :grandparent)]
            (is (= '[parent grandparent]
                   (i/matching-primary-methods dispatcher-with-custom-default method-table :parent)))))))))

(deftest matching-aux-methods-test
  (testing "default aux methods"
    (let [method-table (method-table nil {:before {:child       ['child]
                                                   :parent      ['parent]
                                                   :grandparent ['grandparent]
                                                   :default     ['default]}})]
      (testing "If there are otherwise no matches, default should be returned (but nothing else)"
        (let [dispatcher (impl/standard-dispatcher keyword
                           :hierarchy #'basic-hierarchy)]
          (is (= {:before '[default]}
                 (i/matching-aux-methods dispatcher method-table :cousin)))))

      (testing "default methods should not be included twice if dispatch-value derives from it"
        (let [dispatcher (impl/standard-dispatcher keyword
                           :hierarchy #'basic-hierarchy
                           :default-value :grandparent)]
          (is (= {:before '[parent grandparent]}
                 (i/matching-aux-methods dispatcher method-table :parent))))))))
