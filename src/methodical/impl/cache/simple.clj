(ns methodical.impl.cache.simple
  "A basic, dumb cache. `SimpleCache` stores cached methods in a simple map of dispatch-value -> effective method; it
  offers no facilities to deduplicate identical methods for the same dispatch value. This behaves similarly to the
  caching mechanism in vanilla Clojure."
  (:require [pretty.core :refer [PrettyPrintable]]
            [methodical.interface :as i]))

(deftype SimpleCache [atomm]
  PrettyPrintable
  (pretty [_]
    '(simple-cache))

  i/Cache
  (cached-method [_ dispatch-value]
    (get @atomm dispatch-value))

  (cache-method! [_ dispatch-value method]
    (swap! atomm assoc dispatch-value method))

  (clear-cache! [this]
    (reset! atomm {})
    this)

  (empty-copy [this]
    (SimpleCache. (atom {}))))
