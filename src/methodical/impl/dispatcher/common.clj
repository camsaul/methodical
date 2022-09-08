(ns methodical.impl.dispatcher.common
  "Utility functions for implementing Dispatchers.")

(defn prefers?
  "True if `x` or one of its ancestors is preferred over `y` or one of its ancestors."
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
  `y` (or one of its ancestors).

  4-arity version does not take the `default-dispatch-value` into account, but 5-arity version does."
  ([hierarchy prefs x y]
   (assert (:parents hierarchy) (format "Not a valid hierarchy: %s" (pr-str hierarchy)))
   (and
    (not= x y)
    (or (prefers? hierarchy prefs x y)
        (isa? hierarchy x y))))

  ([hierarchy prefs default-dispatch-value x y]
   (or (dominates? hierarchy prefs x y)
       (and (not= x y)
            (not= x default-dispatch-value)
            (= y default-dispatch-value)))))

(defn domination-comparator
  "Given a `hierarchy` and `prefs` return a function that can be used to sort dispatch values from most-specific to
  least-specific."
  ([dominates?-pred]
   (fn [x y]
     (cond
       (= x y)               0
       (dominates?-pred x y) -1
       (dominates?-pred y x) 1
       :else                 0)))

  ([hierarchy prefs]
   (domination-comparator (partial dominates? hierarchy prefs)))

  ([hierarchy prefs dispatch-value]
   (let [f (domination-comparator hierarchy prefs)]
     (fn [x y]
       (condp = dispatch-value
         x -2
         y 2
         (f x y))))))

(defn ambiguous?
  "True if neither `dispatch-val-x` nor `dispatch-val-y` dominate one another, e.g. because they are the same value or
  are both equally-specific ancestors."
  [hierarchy prefs dispatch-value dispatch-val-x dispatch-val-y]
  (zero? ((domination-comparator hierarchy prefs dispatch-value) dispatch-val-x dispatch-val-y)))

(defn distinct-by
  "Like `distinct`, but uses value of `(f item)` to determine whether to keep each `item` in the resulting collection."
  [f coll]
  (first
   (reduce
    (fn [[items already-seen? :as acc] item]
      (let [v (f item)]
        (if (already-seen? v)
          acc
          [(conj items item) (conj already-seen? v)])))
    [[] #{}]
    coll)))
