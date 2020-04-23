(ns methodical.macros
  "Methodical versions of vanilla Clojure `defmulti` and `defmethod` macros."
  (:refer-clojure :exclude [defmulti defmethod])
  (:require [clojure.string :as str]
            [methodical
             [impl :as impl]
             [interface :as i]
             [util :as u]]
            [pretty.core :as pretty])
  (:import methodical.impl.standard.StandardMultiFn))

(defmacro ^:no-doc defmulti***
  "Impl for `defmulti` macro."
  [name-symb dispatcher & {:keys [hierarchy combo method-table cache]
                           :or   {combo        `(impl/thread-last-method-combination)
                                  method-table `(impl/standard-method-table)
                                  cache        (if hierarchy
                                                 `(impl/watching-cache (impl/simple-cache) [~hierarchy])
                                                 `(impl/simple-cache))}}]
  (let [mta {:ns *ns*, :name (list 'quote (with-meta name-symb nil))}]
    `(def ~name-symb
       (let [impl# (impl/standard-multifn-impl ~combo ~dispatcher ~method-table)]
         (impl/multifn impl# ~mta ~cache)))))

(defmacro ^:no-doc defmulti**
  "Impl for `defmulti` macro."
  [name-symb dispatch-fn & {:keys [hierarchy dispatcher], :as options}]
  (let [dispatcher    (or dispatcher
                          (let [dispatcher-options (select-keys options [:hierarchy :default-value :prefers])]
                            `(impl/multi-default-dispatcher ~dispatch-fn ~@(apply concat dispatcher-options))))
        other-options (dissoc options :hierarchy :dispatcher :default-value :prefers)]
    `(defmulti*** ~name-symb ~dispatcher
       ~@(when-not (contains? options :dispatcher)
           [:hierarchy (or hierarchy #'clojure.core/global-hierarchy)])
       ~@(apply concat other-options))))

(defmacro ^:no-doc defmulti*
  "Impl for `defmulti` macro."
  [name-symb & args]
  (let [[docstring & args]            (if (string? (first args))
                                        args
                                        (cons nil args))
        [attr-map & args]             (if (map? (first args))
                                        args
                                        (cons nil args))
        [dispatch-fn & {:as options}] (if (even? (count args))
                                        (cons nil args)
                                        args)
        metadata                      (merge {:tag methodical.impl.standard.StandardMultiFn}
                                             (when docstring {:doc docstring})
                                             attr-map)
        name-symb                     (vary-meta name-symb merge metadata)]
    (assert (or dispatch-fn (:dispatcher options)) "Missing dispatch function!")
    `(defmulti** ~name-symb ~dispatch-fn ~@(apply concat options))))

(defmacro defmulti
  "Creates a new Methodical multimethod named by a Var. Usage of this macro mimics usage of vanilla Clojure `defmulti`,
  and it can be used as a drop-in replacement; it does, however, support a larger set of options. In addition to the
  usual `:default` and `:hierarchy` options, you many specifiy:

  * `:combo` - The method combination to use for this multimethods. Method combinations define how multiple applicable
     methods are combined; which auxiliary methods, e.g. `:before` or `:after` methods, are supported; and whether other
     advanced facilities, such as `next-method`, are available. There are over a dozen method combinations that ship as
     part of Methodical; many are inspired by their equivalents in the Common Lisp Object System. The default method
     combination is the thread-last method combination.

  * `:dispatcher` - The dispatcher handles dispatch values when invoking a multimethod, and whether one dispatch value
     (and thus, whether its corresponding method) is considered to be more-specific or otherwise preferred over another
     dispatch value. The default dispatcher largely mimics the behavior of the Clojure dispatcher, using a single
     hierarchy augmented by a `prefers` table to control dispatch, with one big improvement: when dispatching on
     multiple values, it supports default methods that specialize on some args and use the default for others.
     (e.g. `[String :default]`)

     Note that the `:hierarchy`, `:default-value` and the positional `dispatch-fn` are provided as conveniences for
     creating a default dispatcher; if you pass a `:dispatcher` arg instead, those arguments are not required and will
     be ignored.

  *  `:cache` - controls caching behavior for effective methods. The default simple cache mimics the behavior of vanilla
      Clojure multimethods.

  *  `:method-table` - maintains tables of dispatch value -> primary method and auxiliary method qualifier -> dispatch
     value -> methods. The default implementation is a pair of simple maps.

  The above options comprise the main constituent parts of a Methodical multimethod, and the majority of those parts
  have several alternative implementations available in `methodical.impl`. Defining additional implementations is
  straightforward as well: see `methodical.interface` for more details.

  Other improvements over vanilla Clojure `defmulti`:

  * Evaluating the form a second time (e.g., when reloading a namespace) will *not* redefine the multimethod, unless
    you have modified its form -- unlike vanilla Clojure multimethods, which need to be unmapped from the namespace to
    make such minor tweaks as changing the dispatch function."
  {:arglists     '([name-symb docstring? attr-map? dispatch-fn
                    & {:keys [hierarchy default-value prefers combo method-table cache]}]
                   [name-symb docstring? attr-map? & {:keys [dispatcher combo method-table cache]}])
   :style/indent :defn}
  [name-symb & args]
  (let [varr         (ns-resolve *ns* name-symb)
        old-val      (some->> varr var-get (instance? StandardMultiFn))
        old-hash     (when old-val
                       (-> varr meta ::defmulti-hash))
        current-hash (hash &form)]
    ;; hashes and the like are expanded out into the macro to make what's going on more obvious when you expand it
    `(let [skip-redef?# (and
                         (let [~'old-hash     ~old-hash
                               ~'current-hash ~current-hash]
                           (= ~'old-hash ~'current-hash))
                         (some-> (ns-resolve *ns* '~name-symb) var-get u/multifn?))]
       (when-not skip-redef?#
         (defmulti* ~(vary-meta name-symb assoc ::defmulti-hash current-hash) ~@args)))))


;;;; ### `defmethod`

(defn- dispatch-val-name
  "Generate a name based on a dispatch value. Used by `method-fn-name` below."
  [dispatch-val]
  (cond
    (sequential? dispatch-val)
    (str/join "-" (map dispatch-val-name dispatch-val))

    (keyword? dispatch-val)
    (name dispatch-val)

    :else
    (str/replace (munge (str dispatch-val)) #"\." "_")))

(defn- method-fn-name
  "Generate a nice name for a primary or auxiliary method's implementing function. Named functions are used rather than
  anonymous functions primarily to aid in debugging and improve stacktraces."
  ([multifn qualifier dispatch-val]
   (symbol (format "%s-%s-method-%s" (name multifn) (name qualifier) (dispatch-val-name dispatch-val))))

  ([multifn qualifier dispatch-val hashh]
   (symbol (format "%s-%s" (name (method-fn-name multifn qualifier dispatch-val)) hashh))))

(defmacro define-primary-method
  "Define a new primary method. Used primarily as part of the implementation of `defmethod`; prefer that macro to using
  this directly."
  {:style/indent [2 :defn]}
  [multifn-symb dispatch-val & fn-tail]
  (let [multifn (var-get (resolve multifn-symb))
        _       (assert (contains? (i/allowed-qualifiers multifn) nil)
                  (format "Method combination %s does not allow primary methods."
                          (pretty/pretty (i/method-combination multifn))))
        fn-name (method-fn-name multifn "primary" dispatch-val)]
    `(do
       (defn ~(vary-meta fn-name assoc :private true) ~@(i/transform-fn-tail multifn nil fn-tail))
       (u/add-primary-method! (var ~multifn-symb) ~dispatch-val ~fn-name))))

(defn- assert-allows-qualifiers [multifn qualifier]
  (assert (contains? (i/allowed-qualifiers multifn) qualifier)
          (format "Method combination %s does not support %s auxiliary methods."
                  (pretty/pretty (i/method-combination multifn)) qualifier)))

(defmacro ^:no-doc define-aux-method*
  "Impl for `define-aux-method*`."
  [multifn-symb qualifier dispatch-val unique-key fn-name & fn-tail]
  (let [multifn (var-get (resolve multifn-symb))
        _       (assert-allows-qualifiers multifn qualifier)
        fn-tail (i/transform-fn-tail multifn qualifier fn-tail)]
    `(do
       (defn ~(vary-meta fn-name assoc :private true) ~@fn-tail)
       (u/add-aux-method-with-unique-key! (var ~multifn-symb) ~qualifier ~dispatch-val ~fn-name ~unique-key))))

(defmacro define-aux-method
  "Define a new auxiliary method. Used primarily as part of the implementation of `defmethod`; prefer that macro to
  using this directly."
  {:arglists     '([multifn-symb qualifier dispatch-val unique-key? & fn-tail])
   :style/indent :defn}
  [multifn-symb qualifier dispatch-val & args]
  (let [[unique-key & fn-tail] (if (sequential? (first args)) (cons nil args)
                                   args)
        multifn                (var-get (resolve multifn-symb))
        _                      (assert multifn)
        fn-name                (if unique-key
                                 (method-fn-name multifn qualifier dispatch-val unique-key)
                                 (method-fn-name multifn qualifier dispatch-val))
        unique-key             (name (or unique-key (ns-name *ns*)))]
    `(define-aux-method* ~multifn-symb ~qualifier ~dispatch-val ~unique-key ~fn-name ~@fn-tail)))

(defmacro defmethod
  "Define a new multimethod method implementation. Syntax is the same as for vanilla Clojure `defmethod`, but you may
  also define auxiliary methods by passing an optional auxiliary method qualifier before the dispatch value:

    ;; define a new primary method
    (defmethod some-multifn Bird
      [_]
      ...)

    ;; define a new *auxiliary* method
    (defmethod some-multifn :before Toucan
      [_]
      ...)"
  {:arglists     '([multifn-symb dispatch-val & fn-tail]
                   [multifn-symb aux-qualifier dispatch-val unique-key? & fn-tail])
   :style/indent :defn}
  [multifn-symb & args]
  (let [multifn
        (var-get (or (resolve multifn-symb)
                     (throw (IllegalArgumentException. (format "Could not resolve multifn %s" multifn-symb)))))

        [qualifier dispatch-val & fn-tail]
        (if (contains? (disj (i/allowed-qualifiers multifn) nil) (first args))
          args
          (cons nil args))]
    (if qualifier
      `(define-aux-method ~multifn-symb ~qualifier ~dispatch-val ~@fn-tail)
      `(define-primary-method ~multifn-symb ~dispatch-val ~@fn-tail))))
