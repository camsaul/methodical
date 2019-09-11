(ns methodical.impl
  "Convenience constructors for various implementations of the different component parts of a Methodical multifn."
  (:require [methodical.impl.cache
             [simple :as cache.simple]
             [watching :as cache.watching]]
            [methodical.impl.combo
             [clojure :as combo.clojure]
             [clos :as combo.clos]
             [operator :as combo.operator]
             [threaded :as combo.threaded]]
            [methodical.impl.dispatcher
             [everything :as dispatcher.everything]
             [standard :as dispatcher.standard]]
            [methodical.impl.method-table
             [clojure :as method-table.clojure]
             [standard :as method-table.standard]]
            [methodical.impl.multifn
             [cached :as multifn.cached]
             [standard :as multifn.standard]]
            [methodical.impl.standard :as impl.standard]
            [methodical.interface :as i]))

;;;; ### Method Combinations

(defn clojure-method-combination
  "Simple method combination strategy that mimics the way vanilla Clojure multimethods combine methods; that is, to say,
  not at all. Like vanilla Clojure multimethods, this method combination only supports primary methods."
  []
  (combo.clojure/->ClojureMethodCombination))

(defn clos-method-combination
  "Method combination stategy that mimics the standard method combination in the Common Lisp Object System (CLOS).
  Supports `:before`, `:after`, and `:around` auxiliary methods. The values returned by `:before` and `:after` methods
  are ignored. Primary methods and around methods get an implicit `next-method` arg (see Methodical dox for more on
  what this means)."
  []
  (combo.clos/->CLOSStandardMethodCombination))

(defn thread-first-method-combination
  "Similar the the standard CLOS-style method combination, but threads the result of each `:before` and `:after`
  auxiliary methods, as well as the primary method, as the *first* arg of subsequent method invocations."
  []
  (combo.threaded/threading-method-combination :thread-first))

(defn thread-last-method-combination
  "Similar the the standard CLOS-style method combination, but threads the result of each `:before` and `:after`
  auxiliary methods, as well as the primary method, as the *last* arg of subsequent method invocations."
  []
  (combo.threaded/threading-method-combination :thread-last))


;;;; #### CLOS-Inspired Operator Method Combinations

;;; These combinations all work more or less the same way: they invoke *all* applicable primary methods, in order from
;;; most-specific to least specific, reducing the results using the function matching their name, e.g.
;;;
;;;    (reduce + (method-1) (method-2) (method-3)) ; `+` method combination
;;;
;;; The following combinations all share the same constraints: they all support `:around` and `:primary` methods, but
;;; not `:before` or `:after` methods. (The only reason this is the case is because that's how it is in CLOS; there's
;;; no reason they *can't* support `:before`, `:after`, `:between`, :`around-each`, :or any other insane auxiliary
;;; method type; these may be added at some point in the future.
;;;
;;; Because all of these combinations automatically invoke *all* relevant primary methods, like CLOS, their primary
;;; methods *do not* get an implicit `next-method` arg; however, `:around` methods still get it (and are still
;;; required to call it.)

(defn do-method-combination
  "Based on the CLOS `progn` method combination. Sequentially executes *all* applicable primary methods, presumably for
  side-effects, in order from most-specific to least-specific; returns the value returned by the least-specific
  method. `do` method combinations support `:around` auxiliary methods, but not `:before` or `:after` methods."
  []
  (combo.operator/operator-method-combination :do))

(defn min-method-combination
  "Based on the CLOS method combination of the same name. Executes *all* applicable primary methods, returning the
  minimum value returned by any implementation. Like `do` method combinations, `min` supports `:around` auxiliary
  methods, but not `:before` or `:after`."
  []
  (combo.operator/operator-method-combination :min))

(defn max-method-combination
  "Executes *all* applicable primary methods, and returns the maximum value returned by any one implemenation. Same
  constraints as othe CLOS operator-style method combinations."
  []
  (combo.operator/operator-method-combination :max))

(defn +-method-combination
  "Executes *all* applicable primary methods, returnings the sum of the values returned by each method. Same constraints
  as othe CLOS operator-style method combinations."
  []
  (combo.operator/operator-method-combination :+))

(defn seq-method-combination
  "Executes *all* applicable primary methods, from most-specific to least-specific; returns a sequence of results from
  the method invocations. Inspired by CLOS `nconc` and `append` method combinations, but unlike those, this
  combination returns a completely lazy sequence. Like other CLOS-operator-inspired method combinations, this
  combination currently supports `:around` methods, but not `:before` or `:after` methods."
  []
  (combo.operator/operator-method-combination :seq))

(defn concat-method-combination
  "Like the `seq-method-combination`, but concatenates all the results together.

    seq-method-combination : map :: concat-method-combination : mapcat"
  []
  (combo.operator/operator-method-combination :concat))


(defn and-method-combination
  "Invoke *all* applicable primary methods, from most-specific to least-specific; reducing the results as if by `and`.
  Like `and`, this method invocation short-circuits if any implementation returns a falsey value. Otherwise, this
  method returns the value returned by the last method invoked."
  []
  (combo.operator/operator-method-combination :and))

(defn or-method-combination
  "Like the `and` combination, but combines result as if by `or`; short-circuits after the first matching primary method
  returns a truthy value."
  []
  (combo.operator/operator-method-combination :or))


;;;; ### Dispatchers

(defn standard-dispatcher
  "Create a stanadrd Methodical multifn dispatcher. The standard dispatcher replicates the way vanilla Clojure
  multimethods handle multimethod dispatch, with support for a custom `hierarchy`, `default-value` and map of
  `prefers`."
  {:style/indent 1}
  [dispatch-fn & {:keys [hierarchy default-value prefers]
                  :or   {hierarchy     #'clojure.core/global-hierarchy
                         default-value :default
                         prefers       {}}}]
  {:pre [(ifn? dispatch-fn) (var? hierarchy) (map? prefers)]}
  (dispatcher.standard/->StandardDispatcher dispatch-fn hierarchy default-value prefers))

(defn everything-dispatcher
  "A Dispatcher that always considers *all* primary and auxiliary methods to be matches; does not calculate dispatch
  values for arguments when invoking. Dispatch values are still used to sort methods from most- to least- specific,
  using `hierarchy` and map of `prefers`."
  [& {:keys [hierarchy prefers]
      :or   {hierarchy #'clojure.core/global-hierarchy
             prefers   {}}}]
  (dispatcher.everything/->EverythingDispatcher hierarchy prefers))


;;;; ### Method Tables

(defn clojure-method-table
  "Create a new Clojure-style method table. Clojure-style method tables only support primary methods."
  ([]
   (clojure-method-table {}))

  ([m]
   {:pre [(map? m)]}
   (method-table.clojure/->ClojureMethodTable m)))


(defn standard-method-table
  "Create a new standard method table that supports both primary and auxiliary methods."
  ([]
   (standard-method-table {} {}))

  ([primary aux]
   {:pre [(map? primary) (map? aux)]}
   (method-table.standard/->StandardMethodTable primary aux)))


;;; ### Caches

(defn simple-cache
  "Create a basic dumb cache. The simple cache stores"
  ([]
   (simple-cache {}))

  ([m]
   (cache.simple/->SimpleCache (atom m))))

(defn watching-cache
  "Wrap `cache` in a `WatchingCache`, which clears the cache whenever one of the watched `references` (such as vars or
  atoms) changes. Intended primarily for use with 'permanent' MultiFns, such as those created with `defmulti`; this is
  rarely needed or wanted for transient multifns."
  [cache references]
  (cache.watching/add-watches cache references))


;;; ### MultiFn Impls

(defn standard-multifn-impl
  "Create a basic multifn impl using method combination `combo`, dispatcher `dispatcher`, and `method-table`."
  [combo dispatcher method-table]
  {:pre [(satisfies? i/MethodCombination combo)
         (satisfies? i/Dispatcher dispatcher)
         (satisfies? i/MethodTable method-table)]}
  (multifn.standard/->StandardMultiFnImpl combo dispatcher method-table))

(defn default-multifn-impl
  "Create a basic multifn impl using default choices for method combination, dispatcher, and method table."
  {:arglists '([dispatch-fn & {:keys [hierarchy default-value prefers]}])}
  [dispatch-fn & dispatcher-options]
  (standard-multifn-impl
   (thread-last-method-combination)
   (apply standard-dispatcher dispatch-fn dispatcher-options)
   (standard-method-table)))

(defn clojure-multifn-impl
  "Create a mulitfn impl that largely behaves the same way as a vanilla Clojure multimethod."
  {:arglists '([dispatch-fn & {:keys [hierarchy default-value prefers method-table]}])}
  [dispatch-fn & {:keys [method-table], :or {method-table {}}, :as options}]
  (let [dispatcher-options (apply concat (select-keys options [:hierarchy :default-value :prefers]))]
    (standard-multifn-impl
     (clojure-method-combination)
     (apply standard-dispatcher dispatch-fn dispatcher-options)
     (clojure-method-table method-table))))

(defn clos-multifn-impl
  "Convenience for creating a new multifn instances that for the most part mimics the behavior of CLOS generic functions
  using the standard method combination. Supports `:before`, `:after`, and `:around` auxiliary methods, but values of
  `:before` and `:after` methods are ignored, rather than threaded. Primary and `:around` methods each get an implicit
  `next-method` arg."
  {:arglists '([dispatch-fn & {:keys [hierarchy default-value prefers primary-method-table aux-method-table]}])}
  [dispatch-fn & {:keys [primary-method-table aux-method-table],
                  :or   {primary-method-table {}, aux-method-table {}}
                  :as   options}]
  (let [dispatcher-options (apply concat (select-keys options [:hierarchy :default-value :prefers]))]
    (standard-multifn-impl
     (clos-method-combination)
     (apply standard-dispatcher dispatch-fn dispatcher-options)
     (standard-method-table primary-method-table aux-method-table))))

(defn cached-multifn-impl
  "Wrap a `MultiFnImpl` in a `CachedMultiFnImpl`, which adds caching to calculated effective methods. The cache itself
  is swappable with other caches that implement different strategies."
  ([impl]
   (cached-multifn-impl impl (simple-cache)))

  ([impl cache]
   (multifn.cached/->CachedMultiFnImpl impl cache)))


;;; Standard MultiFn

(defn uncached-multifn
  "Create a new Methodical multifn using `impl` as the multifn implementation; `impl` itself should implement
  `MultiFnImpl`. DOES NOT CACHE EFFECTIVE METHODS -- use `multifn` instead, unless you like slow dispatch times."
  ([impl]
   (uncached-multifn impl nil))

  ([impl mta]
   (impl.standard/->StandardMultiFn impl mta)))

(defn multifn
  "Create a new *cached* Methodical multifn using `impl` as the multifn implementation."
  ([impl]
   (multifn impl nil))

  ([impl mta]
   (multifn impl mta (simple-cache)))

  ([impl mta cache]
   (uncached-multifn (cached-multifn-impl impl cache) mta)))

(def ^{:arglists (:arglists (meta #'default-multifn-impl))}
  default-multifn
  "Create a new Methodical multifn using the default impl."
  (comp multifn default-multifn-impl))
