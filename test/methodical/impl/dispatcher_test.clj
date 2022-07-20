(ns methodical.impl.dispatcher-test
  "Tests that run against all of our dispatchers."
  (:require [clojure.test :as t]
            [methodical.core :as m]))

(t/deftest allow-non-var-hierarchy-test
  (t/testing ":hierarchy should not have to be a var (#85)"
    (let [hierarchy (atom (-> (make-hierarchy)
                              (derive :parroty :parrot)
                              (derive :parrot :bird)))]
      (doseq [[dispatcher-message dispatcher] {"everything dispatcher"
                                               (m/everything-dispatcher :hierarchy hierarchy)

                                               "standard dispatcher"
                                               (m/standard-dispatcher keyword :hierarchy hierarchy)

                                               "multi-default-dispatcher"
                                               (m/multi-default-dispatcher keyword :hierarchy hierarchy)}]
        (t/testing dispatcher-message
          (let [multifn (-> (m/multifn
                             (m/standard-multifn-impl
                              (m/thread-last-method-combination)
                              dispatcher
                              (m/standard-method-table)))
                            (m/add-primary-method :parrot (fn parrot-method [_next-method k]
                                                            {:parrot k}))
                            (m/add-primary-method :bird (fn bird-method [_next-method k]
                                                          {:bird k})))]
            (t/is (= {:parrot :parroty}
                     (multifn :parroty)))))))))

(t/deftest list-composite-dispatch-values-test
  (t/testing "Dispatch values should match if they are EQUAL (`=`) if we mix and match vectors/lists (#31)"
    ;; this means `[:bird String]` and `(:bird String)` should be treated as the same. However,
    ;;
    ;;    (isa? [:bird String] '(:bird Object))
    ;;
    ;; or vice versa does not hold true. Only vectors should do recursive `isa?` matching -- this is what vanilla
    ;; multimethods do.
    (doseq [dispatch-fn                     [(fn [x y]
                                               (list (keyword x) (class y)))
                                             (fn [x y]
                                               [(keyword x) (class y)])]
            method-dispatch-value           [[:bird String]
                                             (list :bird String)]
            [dispatcher-message dispatcher] {"everything dispatcher"
                                             (m/everything-dispatcher)

                                             "standard dispatcher"
                                             (m/standard-dispatcher dispatch-fn)

                                             "multi-default-dispatcher"
                                             (m/multi-default-dispatcher dispatch-fn)}]
      (t/testing (str \newline dispatcher-message)
        (t/testing (format "\ndispatch value => %s" (pr-str (dispatch-fn :bird "chirp")))
          (t/testing (str \newline (pr-str (list 'add-primary-method method-dispatch-value '...)))
            (let [multifn (-> (m/multifn
                               (m/standard-multifn-impl
                                (m/thread-last-method-combination)
                                dispatcher
                                (m/standard-method-table)))
                              (m/add-primary-method method-dispatch-value (fn String-method [_next-method x y]
                                                                            {:bird x, String y}))
                              (m/add-primary-method [:bird Object] (fn Object-method [_next-method x y]
                                                                     {:bird x, Object y})))]
              (t/is (= {:bird :bird, String "chirp"}
                       (multifn :bird "chirp"))))))))))
