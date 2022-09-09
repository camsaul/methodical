(ns methodical.impl.cache.watching-test
  (:require [clojure.test :as t]
            [methodical.impl.cache.watching :as cache.watching]
            methodical.interface
            [pretty.core :as pretty])
  (:import methodical.impl.cache.watching.WatchingCache
           methodical.interface.Cache))

(set! *warn-on-reflection* true)

(comment methodical.interface/keep-me)

(defn- cache
  [& [num-times-cleared]]
  (reify
    Cache
    (clear-cache! [_]
      (some-> num-times-cleared (swap! inc)))

    pretty/PrettyPrintable
    (pretty [_]
      '(a-cache))))

(t/deftest print-test
  (t/is (= "(watching-cache (a-cache) watching #'clojure.core/+)"
           (pr-str (cache.watching/add-watches (cache) [#'+])))
        "Printing a WatchingCache should give you a nice string describing what it's watching"))

(t/deftest add-watches-test
  (t/testing "If you attempt to create a watching cache with nothing to watch, it should return the cache as-is."
    (let [cache (cache)]
      (t/is (= cache
               (cache.watching/add-watches cache [])))))

  (t/testing "Creating a new WatchingCache should add watches to whatever it references."
    (let [a     (atom nil)
          cache (cache.watching/add-watches (cache) [a])]
      (t/is (= [(.watch-key ^WatchingCache cache)]
               (keys (.getWatches ^clojure.lang.Atom a))))))

  (t/testing "watching-cache should throw an Exception if you try to pass invalid args."
    (t/is (thrown-with-msg? IllegalArgumentException #"Don't know how to create ISeq"
                            (cache.watching/add-watches (cache) #'clojure.core/global-hierarchy)))

    (t/is (thrown-with-msg? AssertionError #"Assert failed"
                            (cache.watching/add-watches (cache) [:a :b :c]))))

  (t/testing "Passing another WatchingCache to add-watches should return a new, flattened WatchingCache."
    (let [[a1 a2]                         (repeatedly 2 #(atom nil))
          cache                           (cache)
          ^WatchingCache watching-cache-1 (cache.watching/add-watches cache [a1])
          ^WatchingCache watching-cache-2 (cache.watching/add-watches watching-cache-1 [a2])]
      (t/is (= #{a1}
               (.refs watching-cache-1)))
      (t/is (= #{a1 a2}
               (.refs watching-cache-2)))
      (t/is (= cache
               (.cache watching-cache-1)))
      (t/is (= cache
               (.cache watching-cache-2)))
      (t/is (= #{(.watch-key watching-cache-1) (.watch-key watching-cache-2)}
               (set (keys (.getWatches ^clojure.lang.Atom a1))))
            "...but it should not remove the watches of the original cache.")
      (t/is (= #{(.watch-key watching-cache-2)}
               (set (keys (.getWatches ^clojure.lang.Atom a2)))))))

  (t/testing "Calling add-watches with a WatchingCache with the same set of refs should return the cache as is."
    (let [a     (atom nil)
          cache (cache.watching/add-watches (cache) [a])]
      (t/is (identical?
             cache
             (cache.watching/add-watches cache [a])))))

  (t/testing "add-watches should flatten recursively."
    (let [[a1 a2 a3] (repeatedly 3 #(atom nil))
          cache      (-> (cache)
                         (cache.watching/add-watches [a1])
                         (cache.watching/add-watches [a2])
                         (cache.watching/add-watches [a3]))]

      (t/is (= #{a1 a2 a3}
               (.refs ^WatchingCache cache)))))

  (t/testing "add-watches should ignore duplicate references"
    (let [a     (atom nil)
          cache (cache.watching/add-watches (cache) [a a a])]
      (t/is (= #{a}
               (.refs ^WatchingCache cache))))))

(t/deftest remove-watches-test
  (let [[a1 a2]          (repeatedly 2 #(atom nil))
        cache            (cache)
        watching-cache-1 (cache.watching/add-watches cache [a1])
        watching-cache-2 (cache.watching/add-watches watching-cache-1 [a2])]
    (t/is (= cache
             (cache.watching/remove-watches watching-cache-2))
          "remove-watches should return the cache it originally wrapped.")

    (t/testing "remove-watches should remove all the watches a watching cache has"
      (t/is (= nil
               (seq (.getWatches ^clojure.lang.Atom a2))))))

  (t/testing "remove-watches on something that's not a watching cache should no-op."
    (let [c (cache)]
      (t/is (= c
               (cache.watching/remove-watches c))))))

(defn- watching-cache [references]
  (let [num-times-cleared (atom 0)
        cache             (cache.watching/add-watches
                           (cache num-times-cleared)
                           references)]
    [cache num-times-cleared]))

(t/deftest clear-test
  (t/testing "Cache shouldn't get cleared the first time it's wrapped in a WatchingCache"
    (let [a              (atom nil)
          [_ num-clears] (watching-cache [a])]
      (t/is (= 0
               @num-clears))))

  (t/testing "If watched var(s) are altered, cache should be cleared"
    (let [a              (atom nil)
          [_ num-clears] (watching-cache [a])]
      (reset! a :a)
      (t/is (= 1
               @num-clears))
      (reset! a :b)
      (t/is (= 2
               @num-clears))))

  (t/testing "If watched var(s) are altered but value but new value = old, cache should not be cleared"
    (let [a              (atom :a)
          [_ num-clears] (watching-cache [a])]
      (reset! a :a)
      (t/is (= 0
               @num-clears))))

  (t/testing "We should be able to watch multiple references"
    (let [a1              (atom nil)
          a2              (atom nil)
          [_ num-clears] (watching-cache [a1 a2])]
      (reset! a1 :a)
      (reset! a2 :b)
      (t/is (= 2
               @num-clears)))))
