(ns methodical.impl.dispatcher.standard-test
  (:require [clojure.test :as t]
            [methodical
             [impl :as impl]
             [interface :as i]])
  (:import methodical.interface.MethodTable))

(t/deftest equality-test
  (t/is (= (impl/standard-dispatcher keyword)
           (impl/standard-dispatcher keyword))
        "Two standard dispatchers with the same args should be considered equal.")
  (t/is (= false (= (impl/standard-dispatcher keyword)
                    (impl/standard-dispatcher keyword :default-value :different)))
        "Two standard dispatchers with different args should *not* be considered equal."))

(defn- method-table [primary-methods aux-methods]
  (reify MethodTable
    (primaryMethods [_] primary-methods)
    (auxMethods [_] aux-methods)))

(def ^:private basic-hierarchy
  (-> (make-hierarchy)
      (derive :child :parent)
      (derive :parent :grandparent)))

(t/deftest matching-primary-methods-test
  (let [dispatcher (impl/standard-dispatcher keyword :hierarchy #'basic-hierarchy)]
    (t/testing "matching-primary-methods should return all matches in order of specificity."
      (let [method-table (method-table {:child 'child, :parent 'parent, :grandparent 'grandparent} nil)]
        (t/is (= '[child parent grandparent]
                 (i/matching-primary-methods dispatcher method-table :child)))

        (t/is (= '[parent grandparent]
                 (i/matching-primary-methods dispatcher method-table :parent)))))

    (t/testing "default primary methods"
      (let [method-table (method-table {:child       'child
                                        :parent      'parent
                                        :grandparent 'grandparent
                                        :default     'default} nil)]
        (t/is (= '[parent grandparent default]
                 (i/matching-primary-methods dispatcher method-table :parent))
              "default methods should be included if they exist")

        (t/is (= '[default]
                 (i/matching-primary-methods dispatcher method-table :cousin))
              "If there are otherwise no matches, default should be returned (but nothing else)")

        (t/testing "default methods should not be included twice if dispatch-value derives from it"
          (let [dispatcher-with-custom-default (impl/standard-dispatcher keyword
                                                                         :hierarchy #'basic-hierarchy
                                                                         :default-value :grandparent)]
            (t/is (= '[parent grandparent]
                     (i/matching-primary-methods dispatcher-with-custom-default method-table :parent)))))))))

(t/deftest matching-aux-methods-test
  (t/testing "default aux methods"
    (let [method-table (method-table nil {:before {:child       ['child]
                                                   :parent      ['parent]
                                                   :grandparent ['grandparent]
                                                   :default     ['default]}})]
      (t/testing "If there are otherwise no matches, default should be returned (but nothing else)"
        (let [dispatcher (impl/standard-dispatcher keyword
                                                   :hierarchy #'basic-hierarchy)]
          (t/is (= {:before '[default]}
                   (i/matching-aux-methods dispatcher method-table :cousin)))))

      (t/testing "default methods should not be included twice if dispatch-value derives from it"
        (let [dispatcher (impl/standard-dispatcher keyword
                                                   :hierarchy #'basic-hierarchy
                                                   :default-value :grandparent)]
          (t/is (= {:before '[parent grandparent]}
                   (i/matching-aux-methods dispatcher method-table :parent))))))))
