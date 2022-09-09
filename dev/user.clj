(ns user
  (:require
   [humane-are.core :as humane-are]
   [pjstadig.humane-test-output :as humane-test-output]))

(humane-test-output/activate!)
(humane-are/install!)
