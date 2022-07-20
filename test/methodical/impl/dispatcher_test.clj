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
