(ns methodical.impl.cache.simple
  "A basic, dumb cache. `SimpleCache` stores cached methods in a simple map of dispatch-value -> effective method; it
  offers no facilities to deduplicate identical methods for the same dispatch value. This behaves similarly to the
  caching mechanism in vanilla Clojure."
  (:require [potemkin.types :as p.types]
            [pretty.core :refer [PrettyPrintable]])
  (:import methodical.interface.Cache))

(p.types/deftype+ SimpleCache [atomm]
  PrettyPrintable
  (pretty [_]
    '(simple-cache))

  Cache
  (cachedMethod [_ dispatch-value]
    (get @atomm dispatch-value))

  (cacheMethodBang [_ dispatch-value method]
    (swap! atomm assoc dispatch-value method))

  (clearCacheBang [this]
    (reset! atomm {})
    this)

  (emptyCopy [this]
    (SimpleCache. (atom {}))))
