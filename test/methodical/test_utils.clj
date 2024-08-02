(ns methodical.test-utils
  (:require
   [clojure.walk :as walk]
   [methodical.util :as u]))

(defn unwrap-fns-with-meta
  "Walk the object and unwrap all encountered FnWithMeta's."
  [coll]
  (walk/postwalk u/unwrap-fn-with-meta coll))
