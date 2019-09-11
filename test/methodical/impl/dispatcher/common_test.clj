(ns methodical.impl.dispatcher.common-test
  (:require [clojure.test :refer :all]
            [methodical.impl.dispatcher.common :as dispatcher.common]))

(deftest prefers-test
  (testing "prefers?"
    (let [h (-> (make-hierarchy)
                (derive :x :x-parent)
                (derive :y :y-parent))]
      (are [msg prefs] (testing (format "x should be preferred over y with prefers = %s" prefs)
                         (is (= true
                                (dispatcher.common/prefers? h prefs :x :y))
                             msg))
        "x is directly preferred over y"
        {:x #{:y}}
        "x is preferred over an ancestor of y"
        {:x #{:y-parent}}
        "an ancestor of x is preferred over y"
        {:x-parent #{:y}}
        "an ancestor of x is preferred over an ancestor of y"
        {:x-parent #{:y-parent}}))))
