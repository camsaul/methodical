(ns methodical.impl.cache.watching
  "A [[methodical.interface/Cache]] implementation that wraps any other cache, watching one or more references (such as an
  atom or var), calling [[methodical.interface/clear-cache!]] whenever one of those references changes.

  `WatchingCache`s can be created by calling [[add-watches]] on another cache. [[add-watches]] is composable, meaning
  you can thread multiple calls to it to build a cache that watches the entire world go by. You could, for example, use
  this to build a multifn that supports a dynamic set of hierarchies, letting you add more as you go. The world's your
  oyster!

  `WatchingCache`s' watch functions weakly reference their caches, meaning they do not prevent garbage collection of
  potentially large method maps; they also automatically clear out their watches when they are garbage collected and
  finalized (which, of course, may actually be never -- but worst-case is that some unneeded calls
  to [[methodical.interface/clear-cache!]] get made)."
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [clojure.datafy :as datafy]
   [methodical.interface :as i]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (java.lang.ref WeakReference)
   (methodical.interface Cache)))

(set! *warn-on-reflection* true)

(declare add-watches remove-watches)

(deftype WatchingCache [^Cache cache watch-key refs]
  pretty/PrettyPrintable
  (pretty [_]
    (concat ['watching-cache cache 'watching] refs))

  Object
  (finalize [this]
    (remove-watches this))

  Cache
  (cached-method [_ dispatch-value]
    (.cached-method cache dispatch-value))

  (cache-method! [this dispatch-value method]
    (.cache-method! cache dispatch-value method)
    this)

  (clear-cache! [this]
    (.clear-cache! cache)
    this)

  (empty-copy [_]
    (add-watches (i/empty-copy cache) refs))

  clojure.protocols/Datafiable
  (datafy [this]
    {:class (class this)
     :cache (datafy/datafy cache)
     :refs  refs})

  describe/Describable
  (describe [this]
    (format "It caches methods using a `%s`." (.getCanonicalName (class this)))))

(defn- cache-watch-fn [cache]
  (let [cache-weak-ref (WeakReference. cache)]
    (fn [_ _ old-value new-value]
      (when-let [cache (.get cache-weak-ref)]
        (when-not (= old-value new-value)
          (i/clear-cache! cache))))))

(defn- new-cache-with-watches
  ^WatchingCache [^Cache wrapped-cache watch-key refs]
  (let [cache    (WatchingCache. wrapped-cache watch-key (set refs))
        watch-fn (cache-watch-fn cache)]
    (doseq [reference refs]
      (add-watch reference watch-key watch-fn))
    cache))

(defn add-watches
  "Create a new cache that watches `refs` (such as vars or atoms), clearing the `cache` it wraps whenever one of the
  watched refs changes.

  *  If `refs` is empty (i.e., there's nothing to watch), or `cache` is already watching the same set of `refs`, this
     function this function returns `cache` as-is.

  * If `cache` is a WatchingCache with a *different* set of refs, this returns a flattened WatchingCache that both the
    original refs and the new ones. The original `cache` is unmodified."
  ^Cache [^Cache cache refs]
  {:pre [(every? (partial instance? clojure.lang.IRef) refs)]}
  (cond
    (empty? refs)
    cache

    (and (instance? WatchingCache cache)
         (= (set refs) (set (.refs ^WatchingCache cache))))
    cache

    (instance? WatchingCache cache)
    (let [^WatchingCache cache cache]
      (recur (.cache cache) (into (set (.refs cache)) refs)))

    :else
    (new-cache-with-watches cache (gensym "watching-cache-") refs)))

(defn remove-watches
  "Recursively removes all watches from `cache`, and returning the cache it wrapped (in case you want to thread it into
  [[add-watches]] to watch something else). If `cache` is not an instance of `WatchingCache`, returns the cache as-is."
  [cache]
  (if-not (instance? WatchingCache cache)
    cache
    (let [^WatchingCache cache cache
          watch-key            (.watch-key cache)]
      (doseq [reference (.refs cache)]
        (remove-watch reference watch-key))
      (recur (.cache cache)))))
