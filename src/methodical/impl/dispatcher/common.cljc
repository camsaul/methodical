(ns methodical.impl.dispatcher.common
  "Utility functions for implementing Dispatchers."
  #?(:cljs
     (:require
      [goog.string :refer [format]])))

#?(:cljs (def ^:private IllegalStateException js/Error))

(defn add-preference
  "Add a method preference to `prefs` for dispatch value `x` over `y`. Used to implement `prefer-method`."
  [isa?* prefs x y]
  (when (= x y)
    (throw (IllegalStateException. (format "Cannot prefer dispatch value %s over itself." x))))
  (when (contains? (get prefs y) x)
    (throw (IllegalStateException. (format "Preference conflict in multimethod: %s is already preferred to %s" y x))))
  ;; this is not actually a restriction that is enforced by vanilla Clojure multimethods, but after thinking about
  ;; it really doesn't seem to make sense to allow you to define a preference that will never be used
  (when (isa?* y x)
    (throw (IllegalStateException.
            (format "Preference conflict in multimethod: cannot prefer %s over its descendant %s."
                    x y))))
  (update prefs x #(conj (set %) y)))

(defn prefers?
  "True if `x` or one of its ancestors is prefered over `y` or one of its ancestors."
  [hierarchy prefs x y]
  (or
   ;; direct preference for x over y
   (contains? (get prefs x) y)
   ;; direct preference for x over one of y's parents (or ancestors, recursively)
   (some
    #(prefers? hierarchy prefs x %)
    (parents hierarchy y))
   ;; direct preference for one of x's parents (or ancestors, recursively) over y
   (some
    #(prefers? hierarchy prefs % y)
    (parents hierarchy x))))

(defn dominates?
  "True if dispatch value `x` should be considered more specific for purposes of method combination over dispatch value
  `y`, e.g. because `x` derives from `y`, or because `x` (or one of its ancestors) has been explicitly preferred over
  `y` (or one of its ancestors)."
  [hierarchy prefs x y]
  (and
   (not= x y)
   (or (prefers? hierarchy prefs x y)
       (isa? hierarchy x y))))

(defn domination-comparitor
  "Given a `hierarchy prefs` return a function that can be used to sort dispatch values from most-specific to
  least-specific."
  [hierarchy prefs dispatch-value]
  (fn [x y]
    (cond
      (= x y)                           0
      (= x dispatch-value)             -2
      (= y dispatch-value)              2
      (dominates? hierarchy prefs x y) -1
      (dominates? hierarchy prefs y x)  1
      :else                             0)))

(defn ambiguous?
  "True if neither `dispatch-val-x` nor `dispatch-val-y` dominate one another, e.g. because they are the same value or
  are both equally-specific ancestors."
  [hierarchy prefs dispatch-value dispatch-val-x dispatch-val-y]
  (zero? ((domination-comparitor hierarchy prefs dispatch-value) dispatch-val-x dispatch-val-y)))
