(ns methodical.util-test
  (:require
   [clojure.test :as t]
   [clojure.walk :as walk]
   [methodical.core :as m]
   [methodical.impl :as impl]
   [methodical.interface :as i]
   [methodical.util :as u]))

(t/deftest multifn?-test
  (t/is (= false
           (u/multifn? (Object.))))

  (t/is (= false
           (u/multifn? nil)))

  (t/is (= true
           (u/multifn? (m/default-multifn keyword)))))

(defn- test-multifn []  (let [m1 'm1
        m2 'm2]
    (-> (m/default-multifn class)
        (m/add-primary-method CharSequence m1)
        (m/add-primary-method :default m2)
        (m/add-aux-method :around :default (fn [next-method x] (next-method x))))))

(t/deftest primary-method-test
  (let [f (test-multifn)]
    (t/testing "primary-method"
      (t/testing "primary-method should return primary methods with exactly the same dispatch value."
        (t/is (= 'm1
                 (u/primary-method f CharSequence))))
      (t/testing "`primary-method` should not return default or parent primary methods -- just the exact match."
        (t/is (= nil
                 (u/primary-method f String))))
      (t/testing "Should return identical methods for multiple calls"
        (t/is (identical? (u/primary-method f CharSequence)
                          (u/primary-method f CharSequence)))))))

(t/deftest applicable-primary-method-test
  (let [f (test-multifn)]
    (t/testing "applicable-primary-method should give you the primary method that will be used for a dispatch value."
      (t/is (= 'm1
               (u/applicable-primary-method f String)))
      (t/testing "Should include dispatch value metadata"
        (t/is (= {:dispatch-value CharSequence}
                 (meta (u/applicable-primary-method f String))))
        (t/is (= {:dispatch-value :default}
                 (meta (u/applicable-primary-method f Integer)))))
      (t/testing "Should return identical methods for multiple calls"
        (t/is (identical? (u/applicable-primary-method f String)
                          (u/applicable-primary-method f CharSequence)))))))

(t/deftest effective-primary-method-test
  (let [f (test-multifn)]
    (t/testing "effective-primary-method should give you the combined effective primary method."
      (let [f (m/add-aux-method f :before Object (fn [x] (conj x :before)))]
        ;; ('m1 next-method ::not-found) -> ::not-found
        (t/is (= ::not-found
                 ((u/effective-primary-method f String) ::not-found)))))
    (t/testing "Should include dispatch value metadata"
      (t/is (= {:dispatch-value :default}
               (meta (u/effective-primary-method f Integer))))
      (t/is (= {:dispatch-value CharSequence}
               (meta (u/effective-primary-method f String))))))
  (t/testing "no matching effective method"
    (t/is (= nil
             (u/effective-primary-method (m/default-multifn class) :wow)))))

;; aux-methods
(t/deftest aux-methods-test
  (t/testing "aux-methods"
    (let [m1 #(conj % :before-1)
          m2 #(conj % :before-2)
          m3 #(conj % :before-3)
          f  (-> (m/default-multifn class)
                 (m/add-aux-method :before String m1)
                 (m/add-aux-method :before String m2)
                 (m/add-aux-method :before Object m3)
                 (m/add-aux-method :after String m2)
                 (m/add-aux-method :after Object m3))]
      (letfn [(replace-fns-with-dispatch-value-metadata [form]
                (walk/postwalk
                 (fn [form]
                   (if (fn? form)
                     (:dispatch-value (meta form))
                     form))
                 form))]
        (t/testing "3-arity"
          (t/is (= [String String]
                   (replace-fns-with-dispatch-value-metadata
                    (u/aux-methods f :before String)))))
        (t/testing "2-ariy"
          (t/is (= {:before [String String]
                    :after  [String]}
                   (replace-fns-with-dispatch-value-metadata
                    (u/aux-methods f String)))))
        (t/testing "1-arity"
          (t/is (= {:before {String [String String]
                             Object [Object]}
                    :after  {String [String]
                             Object [Object]}}
                   (replace-fns-with-dispatch-value-metadata
                    (u/aux-methods f)))))))))

(t/deftest default-methods-test
  (let [m1 (constantly [:char-sequence])
        m2 (constantly [:default])
        f  (-> (m/default-multifn class)
               (m/add-primary-method CharSequence m1)
               (m/add-primary-method :default m2))]
    (t/testing "default-primary-method"
      (t/testing "should be able to get the default primary method"
        (t/is (= [:default]
                 ((u/default-primary-method f))))))

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

(def ^:private lots-of-args-multifn
  (-> (m/default-multifn
       (fn [a b c d e _f] [a (class b) c d e]))
      (m/add-primary-method :default
                            (fn [_ a _ _ _ _ f] {:a a, :f f}))
      (m/add-primary-method [::x :default :default :default :default]
                            (fn [_ a _ _ _ _ f] {:x a, :f f}))))

(t/deftest lots-of-args-test
  (t/is (= {:a :a, :f :f}
           (lots-of-args-multifn :a :b :c :d :e :f)))
  (t/is (= {:x ::x, :f :f}
           (lots-of-args-multifn ::x :b :c :d :e :f))))

(t/deftest dispatch-value-test
  (t/testing "dispatch-value should return the dispatch value of arg(s)"
    (let [f (m/default-multifn keyword)]
      (t/is (= :wow
               (u/dispatch-value f "wow"))))
    (t/testing "2-4 args"
      (let [f (-> (m/default-multifn vector)
                  (m/add-primary-method :default (fn [& args] (vec args))))]
        (t/is (= [:a]
                 (u/dispatch-value f :a)))
        (t/is (= [:a :b]
                 (u/dispatch-value f :a :b)))
        (t/is (= [:a :b :c]
                 (u/dispatch-value f :a :b :c)))
        (t/is (= [:a :b :c :d]
                 (u/dispatch-value f :a :b :c :d)))))
    (t/testing "> 4 args"
      (t/is [::x clojure.lang.Keyword :c :d :e]
            (u/dispatch-value lots-of-args-multifn ::x :b :c :d :e :f)))))

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
      (doseq [[dv expected] {::dog      :default
                             ::bird     ::bird
                             ::parrot   ::parrot
                             ::parakeet ::parrot}]
        (t/testing dv
          (t/is (= expected
                   (m/effective-dispatch-value f dv)))))))
  (t/testing "composite dispatch value"
    (let [f (-> (m/default-multifn (fn [x y] [x (:type y)]))
                (m/add-primary-method :default (fn [_ _ m] m))
                (m/add-aux-method :before [:default ::bird] (fn [_ m] (assoc m :bird? true)))
                (m/add-aux-method :before [Object :default] (fn [_ m] (assoc m :object? true))))]
      (t/is (= [Object :default]
               (m/effective-dispatch-value f [Object ::shoe])
               (m/effective-dispatch-value f [Object :default])))
      (t/is (= [Object ::bird]
               (m/effective-dispatch-value f [Object ::parrot])
               (m/effective-dispatch-value f [String ::parrot])
               (m/effective-dispatch-value f [Object ::parakeet])
               (m/effective-dispatch-value f [String ::parakeet])))
      (t/is (= [:default ::bird]
               (m/effective-dispatch-value f [nil ::parrot])
               (m/effective-dispatch-value f [:default ::parrot])
               (m/effective-dispatch-value f [nil ::parakeet])
               (m/effective-dispatch-value f [:default ::parakeet])))
      (t/is (= :default
               (m/effective-dispatch-value f :default)
               (m/effective-dispatch-value f [nil :default])
               (m/effective-dispatch-value f [:default nil])
               (m/effective-dispatch-value f [:default :default])
               (m/effective-dispatch-value f [:default ::shoe])
               (m/effective-dispatch-value f [nil ::shoe])))))
  (t/testing "> 4 args"
    (t/is [::x :default :default :default :default]
          (->> (u/dispatch-value lots-of-args-multifn ::x :b :c :d :e :f)
               (u/effective-dispatch-value lots-of-args-multifn)))))

(t/deftest dispatch-fn-test
  (t/testing "dispatch-fn should return a function that can be used to get the dispatch value of arg(s)"
    (let [f (m/default-multifn keyword)]
      (t/is (= :wow
               ((u/dispatch-fn f) "wow"))))
    (t/testing "> 4 args"
      (t/is [::x clojure.lang.Keyword :c :d :e]
            ((u/dispatch-fn lots-of-args-multifn) ::x :b :c :d :e :f)))))

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

(t/deftest aux-methods-test-2
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

(t/deftest prefer-method-test
  (let [mf (m/default-multifn :k)]
    (t/is (= {:x #{:y}}
             (i/prefers (u/prefer-method mf :x :y))))
    (t/testing "should thrown an Exception if you try to add an illegal preference"
      (t/is (thrown-with-msg?
             IllegalStateException
             (re-pattern "Cannot prefer dispatch value :x over itself.")
             (u/prefer-method mf :x :x)))
      (let [mf (i/with-prefers mf {:x #{:y}})]
        (t/is (thrown-with-msg?
               IllegalStateException
               (re-pattern "Preference conflict in multimethod: :x is already preferred to :y")
               (u/prefer-method mf :y :x))))
      (let [h   (-> (make-hierarchy)
                    (derive :bird :animal)
                    (derive :toucan :bird))
            mf2 (m/default-multifn :k :hierarchy (atom h))]
        (doseq [k [:bird :animal]]
          (t/testing (format "Prefer %s over :toucan" k)
            (t/is (thrown-with-msg?
                   IllegalStateException
                   (re-pattern (format "Preference conflict in multimethod: cannot prefer %s over its descendant :toucan."
                                       k))
                   (u/prefer-method mf2 k :toucan)))))))))

(t/deftest unprefer-method-test
  (let [m (-> (m/default-multifn :k)
              (u/prefer-method :x :y)
              (u/prefer-method :x :z))]
    (t/is (= {:x #{:y :z}}
             (i/prefers m)))
    (t/testing "Should be able to remove a preference from a multifn"
      (let [m2 (u/unprefer-method m :x :y)]
        (t/is (= {:x #{:z}}
                 (i/prefers m2)))
        (t/testing "Original multimethod should be unaffected"
          (t/is (= {:x #{:y :z}}
                   (i/prefers m))))
        (t/testing "If this was the last preference for x, remove the entry for x"
          (let [m3 (u/unprefer-method m2 :x :z)]
            (t/is (= {}
                     (i/prefers m3)))))))
    (t/testing "Should no-op if preference does not exist"
      (let [m2 (u/unprefer-method m :y :x)]
        (t/is (= {:x #{:y :z}}
                 (i/prefers m2)))
        (t/testing "Original multifn should have been returned"
          (t/is (identical? m m2)))))))

(t/deftest unprefer-method!-test
  (def unprefer-method-mf nil)
  (m/defmulti unprefer-method-mf :k)
  (m/prefer-method! #'unprefer-method-mf :x :y)
  (m/prefer-method! #'unprefer-method-mf :x :z)
  (t/is (= {:x #{:y :z}}
           (i/prefers unprefer-method-mf)))
  (t/testing "No-op if no such preference exists"
    (u/unprefer-method! #'unprefer-method-mf :y :x)
    (t/is (= {:x #{:y :z}}
             (i/prefers unprefer-method-mf))))
  (t/testing "Destructively remove a preference"
    (u/unprefer-method! #'unprefer-method-mf :x :y)
    (t/is (= {:x #{:z}}
             (i/prefers unprefer-method-mf)))))

(t/deftest remove-all-preferences-test
  (let [m (-> (m/default-multifn :k)
              (u/prefer-method :x :y)
              (u/prefer-method :x :z))]
    (t/is (= {:x #{:y :z}}
             (i/prefers m)))
    (let [m2 (u/remove-all-preferences m)]
      (t/is (= {}
               (i/prefers m2)))
      (t/testing "Original multifn should be unaffected"
        (t/is (= {:x #{:y :z}}
                 (i/prefers m))))))
  (t/testing "Should no-op if there are no preferences"
    (let [m (m/default-multifn :k)]
      (t/is (= {}
               (i/prefers m)))
      (let [m2 (u/remove-all-preferences m)]
        (t/is (= {}
                 (i/prefers m2)))
        (t/is (identical? m m2))))))

(t/deftest remove-all-preferences!-test
  (def remove-all-preferences-mf nil)
  (m/defmulti remove-all-preferences-mf :k)
  (m/prefer-method! #'remove-all-preferences-mf :x :y)
  (m/prefer-method! #'remove-all-preferences-mf :x :z)
  (t/is (= {:x #{:y :z}}
           (i/prefers remove-all-preferences-mf)))
  (t/testing "Destructively remove all preferences"
    (u/remove-all-preferences! #'remove-all-preferences-mf)
    (t/is (= {}
             (i/prefers remove-all-preferences-mf)))))

(t/deftest is-default-effective-method?-test
  (doseq [with-default-method? [true false]]
    (t/testing (format "with-default-method? => %s" with-default-method?)
      (let [mf (cond-> (-> (m/default-multifn keyword)
                           (m/add-primary-method :bird (fn [& _args] :bird))
                           (m/add-primary-method nil (fn [& _args] nil))
                           (m/add-aux-method :around :default (fn [& _args] :whatever))
                           (m/add-aux-method :around :octopus (fn [& _args] :octopus)))
                 with-default-method? (m/add-primary-method :default (fn [& _args] :default)))]
        (t/are [dispatch-value expected] (= expected
                                            (m/is-default-effective-method? mf dispatch-value))
          :default true
          :octopus false
          :rhino   true
          :bird    false
          nil      false)))))

(t/deftest is-default-primary-method?-test
  (doseq [with-default-method? [true false]]
    (t/testing (format "with-default-method? => %s" with-default-method?)
      (let [mf (cond-> (-> (m/default-multifn keyword)
                           (m/add-primary-method :bird (fn [& _args] :bird))
                           (m/add-primary-method nil (fn [& _args] nil))
                           (m/add-aux-method :around :default (fn [& _args] :whatever))
                           (m/add-aux-method :around :octopus (fn [& _args] :octopus)))
                 with-default-method? (m/add-primary-method :default (fn [& _args] :default)))]
        (t/are [dispatch-value expected] (= expected
                                            (m/is-default-primary-method? mf dispatch-value))
          :default true
          :octopus true
          :rhino   true
          :bird    false
          nil      false)))))
