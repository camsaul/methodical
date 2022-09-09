(ns user
  (:require
   [environ.core :as env]
   [humane-are.core :as humane-are]
   [pjstadig.humane-test-output :as humane-test-output]))

(when-not (get env/env :inhumane-test-output)
  (humane-test-output/activate!))

(humane-are/install!)
