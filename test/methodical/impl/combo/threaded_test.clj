(ns methodical.impl.combo.threaded-test
  (:require [clojure.test :as t]
            [methodical.impl.combo.threaded :as combo.threaded]
            [methodical.interface :as i]))

(t/deftest threading-invoker-test
  (t/are [threading expected-2 expected-3 expected-4 expected-5]
      (let [invoker (comp second (combo.threaded/threading-invoker threading))]
        (t/is (= expected-2
                 ((invoker :a :b) list 'acc)))

        (t/is (= expected-3
                 ((invoker :a :b :c) list 'acc)))

        (t/is (= expected-4
                 ((invoker :a :b :c :d) list 'acc)))

        (t/is (= expected-5
                 ((invoker :a :b :c :d :e) list 'acc))))

    :thread-first
    ['acc :b]
    ['acc :b :c]
    ['acc :b :c :d]
    ['acc :b :c :d :e]

    :thread-last
    [:a 'acc]
    [:a :b 'acc]
    [:a :b :c 'acc]
    [:a :b :c :d 'acc]))

(defn- combine-methods [threading-type primary-methods aux-methods]
  (i/combine-methods (combo.threaded/threading-method-combination threading-type) primary-methods aux-methods))

(defn- push-val-onto-acc [method-key acc new-val]
  (conj (vec acc) (cons method-key new-val)))

(defn- make-method [threading-type method-key]
  (fn
    ([v]
     (conj v method-key))

    ([x & more]
     (case threading-type
       :thread-first
       (let [acc x]
         (push-val-onto-acc method-key acc (cons 'acc more)))

       :thread-last
       (let [args    (cons x more)
             butlast (butlast args)
             acc     (last args)]
         (push-val-onto-acc method-key acc (concat butlast ['acc])))))))

(defn- make-primary-method [threading method-key]
  (fn [next-method & args]
    (let [acc (apply (make-method threading method-key) args)]
      (if-not next-method
        acc
        (apply next-method (case threading
                             :thread-first (cons acc (rest args))
                             :thread-last  (concat (butlast args) [acc])))))))

(defn- make-around-method [threading method-key]
  (let [before-xform (make-method threading (keyword (format "%s-before" (name method-key))))
        after-xform  #(push-val-onto-acc (keyword (format "%s-after" (name method-key))) % ['acc])]
    (fn [next-method & args]
      (let [acc  (apply before-xform args)
            args (case threading
                   :thread-first (cons acc (rest args))
                   :thread-last  (concat (butlast args) [acc]))
            acc  (apply next-method args)]
        (after-xform acc)))))

(t/deftest primary-test
  (t/testing "Empty primary methods"
    (t/testing "Combine-methods should return nil if `primary-methods` is empty."
      (t/are [threading] (= nil
                            (combine-methods threading [] {:before [(constantly :before)]}))
        :thread-first
        :thread-last))))

(t/deftest before-test
  (t/are [threading expected-1 expected-4]
      (let [primary-methods [(fn [_ & args]
                               (apply (make-method threading :primary) args))]
            aux-methods     {:before [(make-method threading :before-1)
                                      (make-method threading :before-2)]}
            f (combine-methods threading primary-methods aux-methods)]
        (t/testing threading
          (t/is (= expected-1
                   (f []))
                "Arg should be correctly threaded thru for 1 arg.")

          (t/is (= expected-4
                   (case threading
                     :thread-first (f [] :b :c :d)
                     :thread-last  (f :a :b :c [])))
                "Arg should be correctly threaded thru for 4 args.")))

    :thread-first
    [:before-1 :before-2 :primary]
    '[(:before-1 acc :b :c :d)
      (:before-2 acc :b :c :d)
      (:primary  acc :b :c :d)]

    :thread-last
    [:before-1 :before-2 :primary]
    '[(:before-1 :a :b :c acc)
      (:before-2 :a :b :c acc)
      (:primary  :a :b :c acc)]))

(t/deftest after-test
  (t/are [threading expected-1 expected-4]
      (let [primary-methods [(fn [_ & args]
                               (apply (make-method threading :primary) args))]
            aux-methods     {:after [(make-method threading :after-1)
                                     (make-method threading :after-2)]}
            f (combine-methods threading primary-methods aux-methods)]
        (t/testing threading
          (t/is (= expected-1
                   (f []))
                "Arg should be correctly threaded thru for 1 arg.")

          (t/is (= expected-4
                   (case threading
                     :thread-first (f [] :b :c :d)
                     :thread-last  (f :a :b :c [])))
                "Arg should be correctly threaded thru for 4 args.")))

    :thread-first
    [:primary :after-2 :after-1]
    '[(:primary acc :b :c :d)
      (:after-2 acc :b :c :d)
      (:after-1 acc :b :c :d)]

    :thread-last
    [:primary :after-2 :after-1]
    '[(:primary :a :b :c acc)
      (:after-2 :a :b :c acc)
      (:after-1 :a :b :c acc)]))

(t/deftest everything-test
  (t/testing "Aux methods should get called in the order we expect"
    (t/are [threading expected]
        (let [primary-methods [(make-primary-method threading :primary-1)
                               (make-primary-method threading :primary-2)]
              aux-methods     {:before [(make-method threading :before-1)
                                        (make-method threading :before-2)]
                               :after  [(make-method threading :after-1)
                                        (make-method threading :after-2)]
                               :around [(make-around-method threading :around-1)
                                        (make-around-method threading :around-2)]}
              f (combine-methods threading primary-methods aux-methods)]
          (t/testing threading
            (t/is (= expected
                     (case threading
                       :thread-first (f [] :a :b :c)
                       :thread-last  (f :a :b :c []))))))
      :thread-first
      '[(:around-2-before  acc :a :b :c)
        (:around-1-before  acc :a :b :c)
        (:before-1         acc :a :b :c)
        (:before-2         acc :a :b :c)
        (:primary-1        acc :a :b :c)
        (:primary-2        acc :a :b :c)
        (:after-2          acc :a :b :c)
        (:after-1          acc :a :b :c)
        (:around-1-after   acc)
        (:around-2-after   acc)]

      :thread-last
      '[(:around-2-before  :a :b :c acc)
        (:around-1-before  :a :b :c acc)
        (:before-1         :a :b :c acc)
        (:before-2         :a :b :c acc)
        (:primary-1        :a :b :c acc)
        (:primary-2        :a :b :c acc)
        (:after-2          :a :b :c acc)
        (:after-1          :a :b :c acc)
        (:around-1-after   acc)
        (:around-2-after   acc)])))
