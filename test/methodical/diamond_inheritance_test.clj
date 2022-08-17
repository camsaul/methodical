(ns methodical.diamond-inheritance-test
  (:require  [clojure.test :as t]
             [methodical.core :as m]))

(derive ::can ::thing)
(derive ::bird ::thing)

(derive ::toucan ::can)
(derive ::toucan ::bird)

(t/deftest diamond-inheritance-test
  (t/testing "Effective dispatch values calculated incorrectly for diamond inheritance with aux methods (#91)"
    (letfn [(create-mf []
              (-> (m/default-multifn
                   (fn [x _m]
                     (keyword x)))
                  (m/add-primary-method ::thing
                                        (fn [_next-method _x m]
                                          (assoc m :thing? true)))
                  (m/add-primary-method ::can
                                        (fn [next-method x m]
                                          (next-method x (assoc m :can? true))))
                  (m/add-aux-method :before ::bird
                                    (fn [_x m]
                                      (assoc m :before/bird? true)))
                  (m/add-aux-method :after ::can
                                    (fn [_x m]
                                      (assoc m :after/can? true)))))
            (can [mf]
              (t/testing '(mf ::can {})
                (t/testing (str \newline "\neffective-dispatch-value")
                  (t/is (= ::can
                           (m/effective-dispatch-value mf ::can))))
                (t/is (= {:can? true, :thing? true, :after/can? true}
                         (mf ::can {})))))
            (toucan [mf]
              (t/testing (str \newline '(mf ::toucan {}))
                (t/testing "\neffective-dispatch-value"
                  (t/is (= ::toucan
                           (m/effective-dispatch-value mf ::toucan))))
                (t/is (= {:before/bird? true, :can? true, :thing? true, :after/can? true}
                         (mf ::toucan {})))))]
      (t/testing "\n::can before ::toucan"
        (let [mf (create-mf)]
          (can mf)
          (toucan mf)))
      (t/testing "\n::toucan before ::can"
        (let [mf (create-mf)]
          (toucan mf)
          (can mf))))))
