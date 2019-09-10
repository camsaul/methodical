(ns methodical.profile
  (:require [criterium.core :as criterium]
            [methodical
             [core :as m]
             [impl :as impl]]))

(m/defmulti ^:private methodical-multifn
  :type)

(m/defmethod methodical-multifn :default
  [m]
  (assoc m :called :default))

(m/defmethod methodical-multifn :amazing
  [m]
  (assoc m :called :amazing))

(defmulti ^:private clojure-multimethod :type)

(defmethod clojure-multimethod :default
  [m]
  (assoc m :called :default))

(defmethod clojure-multimethod :amazing
  [m]
  (assoc m :called :amazing))

(defn- plain-fn-1 [m] (assoc m :called :amazing))
(defn- plain-fn-2 [m] (assoc m :called :default))

(def ^:private m  {:type :amazing})
(def ^:private m2 {:type :wow})

(defn- profile []
  (assert (= (methodical-multifn m)  (clojure-multimethod m)  (plain-fn-1 m)))
  (assert (= (methodical-multifn m2) (clojure-multimethod m2) (plain-fn-2 m2)))

  (println "\n\nProfiling plain fns...")
  (criterium/bench
   (do
     (plain-fn-1 m)
     (plain-fn-2 m2)))

  (println "\n\nProfiling methodical...")
  (criterium/bench
   (do (methodical-multifn m)
       (methodical-multifn m2)))

  (println "\n\nProfiling vanilla clojure multimethod...")
  (criterium/bench
   (do
     (clojure-multimethod m)
     (clojure-multimethod m2))))

(def ^:private big-hierarchy
  (let [relationships
        (for [child       [:a :b :c :d :e :f]
              parent      [:g :h :i :j :k :l]
              grandparent [:m :n :o :p :q :r]]
          [child parent grandparent])]
    (reduce
     (fn [h [child parent grandparent]]
       (-> h
           (derive child parent)
           (derive parent grandparent)))
     (make-hierarchy)
     relationships)))

(defmulti big-hierarchy-vanilla
  keyword
  :hierarchy #'big-hierarchy)

(prefer-method big-hierarchy-vanilla :o :p)

(defmethod big-hierarchy-vanilla :p [_] :p)

(m/defmulti big-hierarchy-methodical
  keyword
  :hierarchy #'big-hierarchy)

(m/defmethod big-hierarchy-methodical :p [_] :p)

(def big-hierarchy-methodical-clojure
  (impl/multifn (impl/clojure-multifn-impl keyword :hierarchy #'big-hierarchy)))

(m/defmethod big-hierarchy-methodical-clojure :p [_] :p)

(defn profile-big-hierarchy []
  (assert
   (= (big-hierarchy-vanilla :b)
      (big-hierarchy-methodical :b)
      (big-hierarchy-methodical-clojure :b)
      :p))
  (println "\n\nvanilla clojure")
  (criterium/bench (big-hierarchy-vanilla :b))

  (println "\n\nmethodical")
  (criterium/bench (big-hierarchy-methodical :b))

  (println "\n\nmethodical clojure")
  (criterium/bench (big-hierarchy-methodical-clojure :b)))

(defn -main []
  (println "(profile)")
  (profile)
  (println "(profile-big-hierarchy)")
  (profile-big-hierarchy))
