(ns methodical.impl.dispatcher.standard-test
  (:require
   [clojure.test :as t]
   [methodical.core :as m]
   [methodical.impl :as impl]
   [methodical.interface :as i])
  (:import
   (methodical.interface MethodTable)))

(set! *warn-on-reflection* true)

(t/deftest equality-test
  (t/is (= (impl/standard-dispatcher keyword)
           (impl/standard-dispatcher keyword))
        "Two standard dispatchers with the same args should be considered equal.")
  (t/is (= false (= (impl/standard-dispatcher keyword)
                    (impl/standard-dispatcher keyword :default-value :different)))
        "Two standard dispatchers with different args should *not* be considered equal."))

(defn- method-table [primary-methods aux-methods]
  (reify MethodTable
    (primary-methods [_] primary-methods)
    (aux-methods [_] aux-methods)))

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
        (t/testing "default methods should be included if they exist"
          (t/is (= '[parent grandparent default]
                   (i/matching-primary-methods dispatcher method-table :parent)))
          (t/testing "should return ^:dispatch-value metadata"
            (t/is (= [{:dispatch-value :parent} {:dispatch-value :grandparent} {:dispatch-value :default}]
                     (map meta (i/matching-primary-methods dispatcher method-table :parent))))))

        (t/testing "If there are otherwise no matches, default should be returned (but nothing else)"
          (t/is (= '[default]
                   (i/matching-primary-methods dispatcher method-table :cousin)))
          (t/testing "should return ^:dispatch-value metadata"
            (t/is (= [{:dispatch-value :default}]
                     (map meta (i/matching-primary-methods dispatcher method-table :cousin))))))

        (t/testing "default methods should not be included twice if dispatch-value derives from it"
          (let [dispatcher-with-custom-default (impl/standard-dispatcher keyword
                                                 :hierarchy #'basic-hierarchy
                                                 :default-value :grandparent)]
            (t/is (= '[parent grandparent]
                     (i/matching-primary-methods dispatcher-with-custom-default method-table :parent)))
            (t/testing "should return ^:dispatch-value metadata"
              (t/is (= [{:dispatch-value :parent} {:dispatch-value :grandparent}]
                       (map meta (i/matching-primary-methods
                                  dispatcher-with-custom-default
                                  method-table
                                  :parent)))))))))))

(t/deftest matching-aux-methods-test
  (t/testing "default aux methods"
    (let [method-table (method-table nil {:before {:child       ['child]
                                                   :parent      ['parent]
                                                   :grandparent ['grandparent]
                                                   :default     ['default]}})
          aux-methods-metadata (fn [aux-methods]
                                 (into {} (for [[qualifier fns] aux-methods]
                                            [qualifier (map meta fns)])))]
      (t/testing "If there are otherwise no matches, default should be returned (but nothing else)"
        (let [dispatcher (impl/standard-dispatcher keyword
                                                   :hierarchy #'basic-hierarchy)]
          (t/is (= {:before '[default]}
                   (i/matching-aux-methods dispatcher method-table :cousin)))
          (t/testing "should return ^:dispatch-value metadata"
            (t/is (= {:before [{:dispatch-value :default}]}
                     (aux-methods-metadata (i/matching-aux-methods dispatcher method-table :cousin)))))))

      (t/testing "default methods should not be included twice if dispatch-value derives from it"
        (let [dispatcher (impl/standard-dispatcher keyword
                                                   :hierarchy #'basic-hierarchy
                                                   :default-value :grandparent)]
          (t/is (= {:before '[parent grandparent]}
                   (i/matching-aux-methods dispatcher method-table :parent)))
          (t/testing "should return ^:dispatch-value metadata"
            (t/is (= {:before [{:dispatch-value :parent} {:dispatch-value :grandparent}]}
                     (aux-methods-metadata (i/matching-aux-methods dispatcher method-table :parent))))))))))

(derive ::parroty ::parrot)
(derive ::parroty ::friend)

(m/defmulti ^:private ambiguous-mf
  type)

(m/defmethod ambiguous-mf ::parrot
  [m]
  (assoc m :parrot? true))

(m/defmethod ambiguous-mf ::friend
  [m]
  (assoc m :friend? true))

(t/deftest ambiguous-methods-test
  (t/testing "Ambiguous primary method errors should be meaningful (#126)"
    (t/is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           (re-pattern
            (java.util.regex.Pattern/quote
             (str "methodical.impl.dispatcher.standard-test/ambiguous-mf: "
                  "multiple methods match dispatch value: "
                  ":methodical.impl.dispatcher.standard-test/parroty -> :methodical.impl.dispatcher.standard-test/parrot "
                  "and :methodical.impl.dispatcher.standard-test/friend, "
                  "and neither is preferred.")))
           (ambiguous-mf (vary-meta {} assoc :type ::parroty))))
    (t/testing "Exception info should include location where methods were defined"
      (try
        (ambiguous-mf (vary-meta {} assoc :type ::parroty))
        (t/is (= :here false) "should never get here")
        (catch Exception e
          (t/is (= {:method-1 {:ns             (the-ns 'methodical.impl.dispatcher.standard-test)
                               :file           "methodical/impl/dispatcher/standard_test.clj"
                               :line           106
                               :dispatch-value ::parrot}
                    :method-2 {:ns             (the-ns 'methodical.impl.dispatcher.standard-test)
                               :file           "methodical/impl/dispatcher/standard_test.clj"
                               :line           110
                               :dispatch-value ::friend}}
                   (ex-data e))))))))
