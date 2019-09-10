(ns methodical.impl.dispatcher.common
  "Utility functions for implementing Dispatchers.")

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


(defn dominates?
  "True if `dispatch-val-x` should be considered more specific for purposes of method combination over `dispatch-val-y`,
  e.g. because `x` derives from `y` or because `x` has been preferred over `y`."
  [hierarchy prefs x y]
  (and
   (not= x y)
   (or (contains? (get prefs x) y)
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
