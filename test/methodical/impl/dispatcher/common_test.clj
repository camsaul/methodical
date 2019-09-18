(ns methodical.impl.dispatcher.common-test
  (:require [clojure.test :as t]
            [methodical.impl.dispatcher.common :as dispatcher.common]))

(t/deftest prefers-test
  (t/testing "prefers?"
    (let [h (-> (make-hierarchy)
                (derive :x :x-parent)
                (derive :y :y-parent))]
      (t/are [msg prefs] (t/testing (format "x should be preferred over y with prefers = %s" prefs)
                           (t/is (= true
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
