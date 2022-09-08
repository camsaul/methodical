(ns methodical.impl.combo.clos-test
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [methodical.core :as m]
            [methodical.impl.combo.clos :as combo.clos]
            [methodical.interface :as i]))

(defn- combine-methods [primary-methods aux-methods]
  (i/combine-methods (combo.clos/->CLOSStandardMethodCombination) primary-methods aux-methods))

(defn- make-method-fn
  "Return 3 functions:

  *  `calls`, which returns a sequence of calls made;

  * `make-method`; which makes a method impl that adds its invocation (`(method-key & args)`) to calls, and returns
    its first arg (if any) with `method-key` appended.

  * `record-call!`, which records the invocation (just like `make-method` does, but for cases where you don't want to
     use this.)"
  []
  (let [calls* (atom [])]
    ;; fn names provided for clarity/debugging
    [(fn calls []
       @calls*)

     (fn make-method [method-key]
       (fn [& [first-arg :as args]]
         (swap! calls* conj (cons (symbol method-key) args))
         (conj (vec first-arg) method-key)))

     (fn record-call! [method-key & args]
       (swap! calls* conj (cons (symbol method-key) args)))]))

(defn- make-primary-method
  "Makes a primary method that appends `method-key` (default `:primary`) to its first arg, returning that as the result.
  If it has a `next-method`, wraps all other args like `(method-key arg)` and calls the next method like:

    (let [next-result (apply next-method result (rest args))]
      (conj (vec next-result) :method-key-after))."
  ([make-method]
   (make-primary-method make-method :primary))

  ([make-method method-key]
   (let [f (make-method method-key)]
     (fn [next-method & args]
       (let [result (apply f args)]
         (if next-method
           (let [result' (apply next-method result (for [arg (rest args)]
                                                     (list (symbol method-key) arg)))]
             (conj (vec result') (keyword (str (name method-key) "-after"))))
           result))))))

(t/deftest before-test
  (t/testing "before methods for CLOS method combinations"
    (doseq [args [[]
                  [[]]
                  [[] :v2]
                  [[] :v2 :v3]
                  [[] :v2 :v3 :v4]
                  [[] :v2 :v3 :v4 :v5]]]
      (t/testing (format "%d args" (count args))
        (let [[calls make-method] (make-method-fn)
              f                   (combine-methods
                                   [(make-primary-method make-method)]
                                   {:before [(make-method :before-1)
                                             (make-method :before-2)]})]
          (t/testing "result"
            (t/is (= [:primary]
                     (apply f args))
                  "Return values of before methods should be ignored"))

          (t/testing "calls"
            (t/is (= [(cons 'before-1 args)
                      (cons 'before-2 args)
                      (cons 'primary args)]
                     (calls))
                  "Before methods should be called in order from most-specific to least-specific")))))))

(t/deftest after-test
  (t/testing "after methods for CLOS method combinations"
    (doseq [args [[]
                  [[]]
                  [[] :v2]
                  [[] :v2 :v3]
                  [[] :v2 :v3 :v4]
                  [[] :v2 :v3 :v4 :v5]]]
      (t/testing (format "%d args" (count args))
        (let [[calls make-method] (make-method-fn)
              f                   (combine-methods
                                   [(make-primary-method make-method)]
                                   {:after [(make-method :after-1)
                                            (make-method :after-2)]})]
          (t/testing "result"
            (t/is (= [:primary]
                     (apply f args))
                  "Return values of after methods should be ignored"))

          (t/testing "calls"
            (t/is (= [(cons 'primary args)
                      '(after-2 [:primary])
                      '(after-1 [:primary])]
                     (calls))
                  "after methods should be called in order from least- to most-specific with result of primary fn")))))))

(defn- make-around-method
  "Makes an around method that appends `<method-key>-before` to the first arg (if any), wraps all other args
  in `(<method-key>-before arg)`, calls `next-method`, then appends the `<method-key>-after` to the result."
  [record-call! method-key]
  (let [[before-key after-key] (map #(keyword (str (name method-key) \- %)) ["before" "after"])]
    (fn [next-method & [acc & rest-args :as args]]
      (apply record-call! before-key args)
      (let [acc'       (when (seq args)
                         (conj (vec acc) before-key))
            rest-args' (for [arg rest-args]
                         (list (symbol (name before-key)) arg))
            args'      (when acc' (cons acc' rest-args'))
            result     (apply next-method args')]
        (record-call! after-key result)
        (conj (vec result) after-key)))))

(t/deftest around-test
  (t/testing "around methods"
    (doseq [args [[]
                  [[]]
                  [[] :v2]
                  [[] :v2 :v3]
                  [[] :v2 :v3 :v4]
                  [[] :v2 :v3 :v4 :v5]]]
      (t/testing (format "%d args" (count args))
        (let [[calls make-method record-call!] (make-method-fn)
              f                                (combine-methods
                                                [(make-primary-method make-method)]
                                                {:around [(make-around-method record-call! :around-1)
                                                          (make-around-method record-call! :around-2)]})]
          (t/testing "result"
            (let [expected-args (if (empty? args)
                                  [:primary :around-1-after :around-2-after]
                                  [:around-2-before :around-1-before :primary :around-1-after :around-2-after])]
              (t/is (= expected-args
                       (apply f args))
                    "Around methods should be able to modify args, and modify the results")))

          (t/testing "calls"
            (let [expected-calls (if (empty? args)
                                   '[(around-2-before)
                                     (around-1-before)
                                     (primary)
                                     (around-1-after [:primary])
                                     (around-2-after [:primary :around-1-after])]
                                   [(cons 'around-2-before args)
                                    (concat '(around-1-before [:around-2-before])
                                            (for [arg (rest args)]
                                              (list 'around-2-before arg)))
                                    (concat '(primary [:around-2-before :around-1-before])
                                            (for [arg (rest args)]
                                              (list 'around-1-before (list 'around-2-before arg))))
                                    '(around-1-after [:around-2-before :around-1-before :primary])
                                    '(around-2-after [:around-2-before :around-1-before :primary :around-1-after])])]
              (t/is (= expected-calls
                       (calls))
                    "Around methods should be applied, in or in order from least- to most- specific"))))))))

(t/deftest primary-method-test
  (t/testing "Empty primary-methods"
    (t/is (= nil
             (combine-methods [] {:before [(constantly :before)]}))
          "combine-methods should return nil if there are no matching primary methods."))

  (t/testing "next-method"
    (doseq [args [[]
                  [[]]
                  [[] :v2]
                  [[] :v2 :v3]
                  [[] :v2 :v3 :v4]
                  [[] :v2 :v3 :v4 :v5]]]
      (t/testing (format "%d args" (count args))
        (let [[calls make-method] (make-method-fn)

              f
              (combine-methods [(make-primary-method make-method :primary-1)
                                (make-primary-method make-method :primary-2)]
                               nil)]
          (t/is (= [:primary-1 :primary-2 :primary-1-after]
                   (f []))
                "Calling `next-method` should invoke the next method")

          (t/testing "calls"
            (t/is (= '[(primary-1 [])
                       (primary-2 [:primary-1])]
                     (calls)))))))))

(t/deftest everything-test
  (let [[calls make-method record-call!] (make-method-fn)

        f
        (combine-methods [(make-primary-method make-method :primary-1)
                          (make-primary-method make-method :primary-2)]
                         {:before [(make-method :before-1)
                                   (make-method :before-2)]
                          :after  [(make-method :after-1)
                                   (make-method :after-2)]
                          :around [(make-around-method record-call! :around-1)
                                   (make-around-method record-call! :around-2)]})]
    (t/is (= [:around-2-before :around-1-before :primary-1 :primary-2 :primary-1-after :around-1-after :around-2-after]
             (f []))
          "Results of before/after methods should be ignored")

    (t/is (= '[(around-2-before [])
               (around-1-before [:around-2-before])
               (before-1        [:around-2-before :around-1-before])
               (before-2        [:around-2-before :around-1-before])
               (primary-1       [:around-2-before :around-1-before])
               (primary-2       [:around-2-before :around-1-before :primary-1])
               (after-2         [:around-2-before :around-1-before :primary-1 :primary-2 :primary-1-after])
               (after-1         [:around-2-before :around-1-before :primary-1 :primary-2 :primary-1-after])
               (around-1-after  [:around-2-before :around-1-before :primary-1 :primary-2 :primary-1-after])
               (around-2-after  [:around-2-before :around-1-before :primary-1 :primary-2 :primary-1-after
                                 :around-1-after])]
             (calls))
          "Aux methods should get called in the order we expect")))

(m/defmulti ^:private clos-multifn class
  :combo (m/clos-method-combination))

(m/defmethod clos-multifn Object
  [s]
  (str s "!"))

(m/defmethod clos-multifn clojure.lang.PersistentVector
  [coll]
  (next-method (str/join coll)))

(m/defmethod clos-multifn :around String
  [s]
  (str (next-method s) " <-> " (next-method s)))

(t/deftest e2e-test
  (t/is (= "A! <-> A!"
           (clos-multifn "A")))
  (t/is (= "ABC!"
           (clos-multifn ["A" "B" "C"]))))
