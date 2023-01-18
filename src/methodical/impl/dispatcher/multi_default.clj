(ns methodical.impl.dispatcher.multi-default
  "A single-hierarchy dispatcher similar to the standard dispatcher, with one big improvement: when dispatching on
  multiple values, it supports default methods that specialize on some args and use the default for others. (e.g.
  `[String :default]`"
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [methodical.impl.dispatcher.common :as dispatcher.common]
   [methodical.impl.dispatcher.standard :as dispatcher.standard]
   [methodical.interface :as i]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (methodical.interface Dispatcher)))

(set! *warn-on-reflection* true)

(defn- partially-specialized-default-dispatch-values* [dispatch-value default-value]
  ;; The basic idea here is to count down from (2^(count dispatch-value) - 2) to 0, then treat each bit as whether the
  ;; value at the corresponding position in `dispatch-value` should be included (if the bit is `1`) or if
  ;; `default-value` should be included in its place (if the bit is `0`). e.g. for
  ;;
  ;;     (partially-specialized-default-dispatch-values [:x :y] :default)
  ;;
  ;; then
  ;;
  ;;     (count dispatch-value)` is 2
  ;;     2^count = 4
  ;;
  ;; i.e., count from 2 down to 0. The table below illustrates how this works:
  ;;
  ;;     i | binary | corresponding dispatch val
  ;;     --+--------+---------------------------
  ;;     2 | 10     | [:x :default]
  ;;     1 | 01     | [:default :y]
  ;;     0 | 00     | [:default :default]
  (let [cnt (count dispatch-value)]
    (for [i (reverse (range (dec (int (Math/pow 2 cnt)))))]
      (vec
       (for [j (reverse (range 0 cnt))]
         (if (pos? (bit-and i (bit-shift-left 1 j)))
           (nth dispatch-value (- cnt j 1))
           default-value))))))

(defn partially-specialized-default-dispatch-values
  "Return a sequence of all partially-specialized default dispatch values for a given `dispatch-value` and
  `default-value`, in order from most-specific to least-specific.

  ```clj
  (default-dispatch-values [:x :y] :default)
  ->
  ([:x :default]        ; if no method for [:x :y] exists, look for [:x :default]...
   [:default :y]        ; or [:default :y] ...
   [:default :default])
  ```"
  [dispatch-value default-value]
  (when (and (sequential? dispatch-value)
             (not (sequential? default-value)))
    (partially-specialized-default-dispatch-values* dispatch-value default-value)))

(defn- matching-partially-specialized-default-primary-method-pairs*
  [{:keys [default-value dispatch-value unambiguous-pairs-seq-fn]
    :or   {unambiguous-pairs-seq-fn dispatcher.standard/unambiguous-pairs-seq}
    :as   opts}]
  (mapcat
   (fn [partial-default]
     (let [pairs (dispatcher.standard/matching-primary-pairs-excluding-default
                  (assoc opts :dispatch-value partial-default))]
       (unambiguous-pairs-seq-fn opts pairs)))
   (partially-specialized-default-dispatch-values dispatch-value default-value)))

(defn matching-partially-specialized-default-primary-method-pairs
  "Return pairs of `[dispatch-value method]` for all matching partially-specialized default methods, sorted from
  most-specific to least-specific"
  ;; TODO - this is too many args!
  [opts standard-dispatch-vals]
  (->> (matching-partially-specialized-default-primary-method-pairs* opts)
       (dispatcher.common/distinct-by first)
       (remove
        (fn [[dispatch-val]]
          (contains? standard-dispatch-vals dispatch-val)))))

(defn matching-primary-methods
  "Return a lazy sequence of applicable priamry methods for `dispatch-value`, sorted from most-specific to
  least-specific. Similar to the implementation in [[methodical.impl.dispatcher.standard]], but supports
  partially-specialized default methods; see explanation in ns docstring."
  [{:keys [default-value method-table unambiguous-pairs-seq-fn]
    :or   {unambiguous-pairs-seq-fn dispatcher.standard/unambiguous-pairs-seq}
    :as   opts}]
  {:pre [(some? method-table)]}
  ;; this is basically the same logic as the version in `standard`, but instead `matches + default` we return
  ;; `matches + partial-defaults + default`
  (let [primary-methods        (i/primary-methods method-table)
        opts                   (assoc opts :method-map primary-methods)
        standard-pairs         (dispatcher.standard/matching-primary-pairs-excluding-default opts)
        ;; filter out any partially-specialized default methods that already appear in the standard matches, e.g. if
        ;; dispatch value is something like [:x :default]
        standard-dispatch-vals (set (map first standard-pairs))
        partial-default-pairs  (matching-partially-specialized-default-primary-method-pairs opts standard-dispatch-vals)
        default-pair           (when-not (or (contains? standard-dispatch-vals default-value)
                                             (contains? (set (map first partial-default-pairs)) default-value))
                                 (when-let [default-method (get primary-methods default-value)]
                                   [default-value default-method]))
        pairs                  (concat
                                (unambiguous-pairs-seq-fn opts standard-pairs)
                                partial-default-pairs
                                (when default-pair [default-pair]))]
    (map second (dispatcher.common/distinct-by first pairs))))

(defn- aux-dispatch-values [qualifier {:keys [default-value method-table dispatch-value hierarchy prefs]}]
  (let [comparatorr (dispatcher.common/domination-comparator hierarchy prefs dispatch-value)]
    (distinct
     (sort-by
      identity
      comparatorr
      (for [dispatch-value (concat [dispatch-value]
                                   (partially-specialized-default-dispatch-values dispatch-value default-value)
                                   [default-value])
            dv             (keys (get (i/aux-methods method-table) qualifier))
            :when          (isa? hierarchy dispatch-value dv)]
        dv)))))

(defn- matching-aux-methods*
  [qualifier {:keys [method-table] :as opts}]
  (let [method-map (i/aux-methods method-table)]
    (for [dispatch-value (aux-dispatch-values qualifier opts)
          m              (get-in method-map [qualifier dispatch-value])]
      m)))

(defn matching-aux-methods
  "Impl of `Dispatcher` `matching-aux-methods` for the multi-default dispatcher."
  [{:keys [method-table] :as opts}]
  (into {} (for [[qualifier] (i/aux-methods method-table)]
             [qualifier (matching-aux-methods* qualifier opts)])))

(deftype MultiDefaultDispatcher [dispatch-fn hierarchy-var default-value prefs]
  pretty/PrettyPrintable
  (pretty [_]
    (concat ['multi-default-dispatcher dispatch-fn]
            (when (not= hierarchy-var #'clojure.core/global-hierarchy)
              [:hierarchy hierarchy-var])
            (when (not= default-value :default)
              [:default-value default-value])
            (when (seq prefs)
              [:prefers prefs])))

  Object
  (equals [_ another]
    (and
     (instance? MultiDefaultDispatcher another)
     (let [^MultiDefaultDispatcher another another]
       (and
        (= dispatch-fn   (.dispatch-fn another))
        (= hierarchy-var (.hierarchy-var another))
        (= default-value (.default-value another))
        (= prefs         (.prefs another))))))

  Dispatcher
  (dispatch-value [_]              (dispatch-fn))
  (dispatch-value [_ a]            (dispatch-fn a))
  (dispatch-value [_ a b]          (dispatch-fn a b))
  (dispatch-value [_ a b c]        (dispatch-fn a b c))
  (dispatch-value [_ a b c d]      (dispatch-fn a b c d))
  (dispatch-value [_ a b c d more] (apply dispatch-fn a b c d more))

  (matching-primary-methods [_ method-table dispatch-value]
    (matching-primary-methods
     {:hierarchy      (deref hierarchy-var)
      :prefs          prefs
      :default-value  default-value
      :method-table   method-table
      :dispatch-value dispatch-value}))

  (matching-aux-methods [_ method-table dispatch-value]
    (matching-aux-methods
     {:hierarchy      (deref hierarchy-var)
      :prefs          prefs
      :default-value  default-value
      :method-table   method-table
      :dispatch-value dispatch-value}))

  (default-dispatch-value [_]
    default-value)

  (prefers [_]
    prefs)

  (with-prefers [_this new-prefs]
    (MultiDefaultDispatcher. dispatch-fn hierarchy-var default-value new-prefs))

  (dominates? [_ x y]
    (dispatcher.common/dominates? (deref hierarchy-var) prefs default-value x y))

  clojure.protocols/Datafiable
  (datafy [this]
    {:class         (class this)
     :dispatch-fn   dispatch-fn
     :default-value default-value
     :hierarchy     hierarchy-var
     :prefs         prefs})

  describe/Describable
  (describe [this]
    (format "It uses the dispatcher `%s`\nwith hierarchy `%s`\nand prefs `%s`.\n\nThe default value is `%s`."
            (.getCanonicalName (class this))
            (pr-str hierarchy-var)
            (pr-str prefs)
            (pr-str default-value))))
