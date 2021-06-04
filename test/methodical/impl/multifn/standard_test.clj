(ns methodical.impl.multifn.standard-test
  (:require [clojure.test :as t]
            [methodical.core :as m]
            [methodical.impl.multifn.standard :as standard]))

(t/deftest standard-effective-method-dispatch-value-test
  (t/testing "standard-effective-method should return a method with the correct ^:dispatch-value metadata"
    (derive ::parrot ::bird)
    (derive ::parakeet ::parrot)
    (derive ::budgie ::parakeet)
    (derive ::love-bird ::parrot)
    (let [combo        (m/thread-last-method-combination)
          dispatcher   (m/multi-default-dispatcher :type)
          method-table (-> (m/standard-method-table)
                           (m/add-primary-method :default (fn [_]))
                           (m/add-primary-method ::parakeet (fn [_]))
                           (m/add-aux-method :after ::bird (fn [_]))
                           (m/add-aux-method :after ::parrot (fn [_])))]
      (doseq [[dv expected] {::dog       :default
                             ::bird      ::bird
                             ::parrot    ::parrot
                             ::parakeet  ::parakeet
                             ::budgie    ::parakeet
                             ::love-bird ::parrot}]
        (t/testing dv
          (t/is (= {:dispatch-value expected}
                   (meta (standard/standard-effective-method combo dispatcher method-table dv)))))))))
