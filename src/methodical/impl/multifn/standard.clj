(ns methodical.impl.multifn.standard
  "Standard Methodical MultiFn impl."
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [clojure.datafy :as datafy]
   [methodical.impl.dispatcher.common :as dispatcher.common]
   [methodical.interface :as i]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (methodical.interface Dispatcher MethodCombination MethodTable MultiFnImpl)))

(set! *warn-on-reflection* true)

;; "composite dispatch value" below just means a dispatch value consisting of multiple parts e.g. `[:x :y]` as opposed
;; to a single value like `:x`.

(defn sort-dispatch-values
  "Sort dispatch values in order from most-specific-overall to least-specific-overall."
  [dispatcher dispatch-values]
  (sort-by
   identity
   (dispatcher.common/domination-comparator (partial i/dominates? dispatcher))
   dispatch-values))

(defn non-composite-effective-dispatch-value
  "Operates only on non-composite dispatch values. Determine the effective (most-specific) dispatch value that will be
  used when dispatching on `actual-dispatch-value`. If there is a dispatch value in `method-dispatch-values` that
  dominates all other method dispatch values, that is the effective dispatch value. Otherwise the actual dispatch
  value will be used.

  Example. Suppose a `::toucan` is a `::can`, and a `::toucan` is a `::bird`. If we dispatch off of `::toucan` and
  only have a method for `::bird`, then `::bird` is the effective dispatch value, because there are no other dispatch
  values that are more specific that would cause other methods to be used; the result is the same as if we had
  dispatched off of `::bird` in the first place. However, if we add a `::can` method, the effective dispatch value for
  `::toucan` can no longer be `::bird`, because a `::bird` is not necessarily a `::can`. Thus our effective dispatch
  value would become `::toucan`, since out of the three possibilities only a `::toucan` is both a `::bird` and a
  `::can`."
  [dispatcher actual-dispatch-value method-dispatch-values]
  (let [[most-specific-dispatch-value & more-dispatch-values] (distinct (sort-dispatch-values dispatcher method-dispatch-values))
        ;; do not take preferences into account when calculating the effective dispatch value. Aux methods are applied
        ;; to *all* matching dispatch values, which means that one aux method should not be considered unambiguously
        ;; dominant over another based on preferences alone; we should only consider a method to be dominant over
        ;; another for effective dispatch value purposes if its dispatch value ISA all of the other matching dispatch
        ;; values.
        ;;
        ;; Example: if a `:toucan` ISA `:can` and a `:toucan` ISA `:bird`, and we have `:before` methods for both
        ;; `:can` and `:bird`, and a preference for `:bird` over `:can`, we cannot consider `:bird` to be the
        ;; effective dispatch value for `:toucan`, because a `:toucan` ISA `:can`, but a `:bird` is not a `:can`.
        dispatcher                                            (i/with-prefers dispatcher nil)]
    (if (every? (fn [another-dispatch-value]
                  (i/dominates? dispatcher most-specific-dispatch-value another-dispatch-value))
                more-dispatch-values)
      most-specific-dispatch-value
      actual-dispatch-value)))

(defn composite-effective-dispatch-value
  "Combine multiple composite dispatch values into a single composite dispatch value that has the overall most-specific
  arg for each position, e.g.


  ```clj
  ;; String is more specific than Object; ::parrot is more specific than ::bird
  (composite-effective-dispatch-value [[Object ::parrot] [String ::bird]]) ; -> [String ::parrot]
  ```

  If the most-specific dispatch value is not composite, it returns it directly."
  [dispatcher actual-dispatch-value method-dispatch-values]
  ;; sort the values so in cases where there's ambiguity we take the keep the value in the overall-most-specific
  ;; dispatch value.
  (let [[most-specific-method-dispatch-value :as method-dispatch-values] (sort-dispatch-values dispatcher method-dispatch-values)]
    ;; if the most-specific dispatch value is not composite, we can return it as-is -- there's no need to build a
    ;; composite dispatch value.
    (if-not (sequential? most-specific-method-dispatch-value)
      (non-composite-effective-dispatch-value dispatcher actual-dispatch-value method-dispatch-values)
      ;; otherwise we need to combine stuff
      (mapv (fn [i]
              (non-composite-effective-dispatch-value dispatcher
                                                      (nth actual-dispatch-value i)
                                                      (map #(nth % i)
                                                           (filter sequential? method-dispatch-values))))
            (range (count actual-dispatch-value))))))

(defn effective-dispatch-value
  "Given matching `primary-methods` and `aux-methods` for the `actual-dispatch-value`, determine the effective dispatch
  value."
  [dispatcher actual-dispatch-value primary-methods aux-methods]
  (let [dispatch-values (transduce
                         (comp cat
                               (map meta)
                               (filter #(contains? % :dispatch-value))
                               (map :dispatch-value))
                         conj
                         []
                         (cons primary-methods (vals aux-methods)))]
    (composite-effective-dispatch-value dispatcher actual-dispatch-value dispatch-values)))

(defn standard-effective-method
  "Build an effective method using the 'standard' technique, taking the dispatch-value-method pairs in the
  `method-table`, finiding applicable ones using `dispatcher`, and combining them using `method-combination`."
  [method-combination dispatcher method-table dispatch-value]
  (let [primary-methods (i/matching-primary-methods dispatcher method-table dispatch-value)
        aux-methods     (i/matching-aux-methods dispatcher method-table dispatch-value)]
    (some-> (i/combine-methods method-combination primary-methods aux-methods)
            (with-meta {:dispatch-value (effective-dispatch-value dispatcher dispatch-value primary-methods aux-methods)}))))

(deftype StandardMultiFnImpl [^MethodCombination combo
                              ^Dispatcher dispatcher
                              ^MethodTable method-table]
  pretty/PrettyPrintable
  (pretty [_this]
    (list 'standard-multifn-impl combo dispatcher method-table))

  Object
  (equals [_ another]
    (and (instance? StandardMultiFnImpl another)
         (let [^StandardMultiFnImpl another another]
           (and (= combo (.combo another))
                (= dispatcher (.dispatcher another))
                (= method-table (.method-table another))))))

  MultiFnImpl
  (method-combination [_this]
    combo)

  (dispatcher [_this]
    dispatcher)

  (with-dispatcher [this new-dispatcher]
    (if (= dispatcher new-dispatcher)
      this
      (StandardMultiFnImpl. combo new-dispatcher method-table)))

  (method-table [_this]
    method-table)

  (with-method-table [this new-method-table]
    (if (= method-table new-method-table)
      this
      (StandardMultiFnImpl. combo dispatcher new-method-table)))

  (effective-method [_this dispatch-value]
    (standard-effective-method combo dispatcher method-table dispatch-value))

  clojure.protocols/Datafiable
  (datafy [this]
    {:class        (class this)
     :combo        (datafy/datafy combo)
     :dispatcher   (datafy/datafy dispatcher)
     :method-table (datafy/datafy method-table)})

  describe/Describable
  (describe [_this]
    (str (describe/describe combo)
         \newline \newline
         (describe/describe dispatcher)
         \newline \newline
         (describe/describe method-table))))
