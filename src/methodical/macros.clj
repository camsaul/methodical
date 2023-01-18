(ns methodical.macros
  "Methodical versions of vanilla Clojure [[defmulti]] and [[defmethod]] macros."
  (:refer-clojure :exclude [defmulti defmethod])
  (:require
   [clojure.core.specs.alpha]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [methodical.impl :as impl]
   [methodical.interface :as i]
   [methodical.macros.validate-arities :as validate-arities]
   [methodical.util :as u])
  (:import
   (methodical.impl.standard StandardMultiFn)))

(set! *warn-on-reflection* true)

(comment clojure.core.specs.alpha/keep-me) ; for the specs below

(s/def ::fn-tail
  (s/alt :arity-1 :clojure.core.specs.alpha/params+body
         :arity-n (s/+ (s/spec :clojure.core.specs.alpha/params+body))))

(s/def :methodical.macros.defmulti.attr-map/dispatch-value-spec
  ;; not sure how to validate this. Is there a predicate for something that can be used as a
  ;; spec? [[clojure.spec.alpha/spec]] will happily turn random things into specs for us
  any?)

(s/def :methodical.macros.defmulti.attr-map/defmethod-arities
  (s/nilable ::validate-arities/arities-set))

(s/def :methodical.macros.defmulti/attr-map
  (s/keys :opt-un [:methodical.macros.defmulti.attr-map/dispatch-value-spec
                   :methodical.macros.defmulti.attr-map/defmethod-arities]))

(s/def ::defmulti-args
  (s/& (s/cat :name-symb   (every-pred symbol? (complement namespace))
              :docstring   (s/? string?)
              :attr-map    (s/? map?)
              :dispatch-fn (s/? any?)
              :options     (s/* (s/cat :k keyword?
                                       :v any?)))
       ;; do a "soft" cut here where any map argument is always interpreted as an attribute map, and only later subject
       ;; it to stricter validation. See my questions in Slack here
       ;; https://clojurians.slack.com/archives/C1B1BB2Q3/p1662755403589769
       (s/keys :opt-un [:methodical.macros.defmulti/attr-map])))

(defmacro ^:no-doc redefine-multimethod
  "Impl for the [[defmulti]] macro."
  [name-symb dispatch-fn {:keys [hierarchy dispatcher combo method-table cache default-value]
                          :or   {combo        `(impl/thread-last-method-combination)
                                 method-table `(impl/standard-method-table)
                                 cache        (if hierarchy
                                                `(impl/watching-cache (impl/simple-cache) [~hierarchy])
                                                `(impl/simple-cache))
                                 hierarchy    '#'clojure.core/global-hierarchy}
                          prefs :prefers}]
  (let [dispatch-fn (or dispatch-fn `identity)
        dispatcher  (or dispatcher
                        `(impl/multi-default-dispatcher ~dispatch-fn
                                                        :hierarchy ~hierarchy
                                                        ~@(when default-value
                                                            [:default-value default-value])
                                                        ~@(when prefs
                                                            [:prefers prefs])))
        ;; attach the var metadata to the multimethod itself as well so we can use it for cool stuff e.g.
        ;; `:dispatch-value-spec` or `:arglists`.
        mta         (merge (meta name-symb)
                           {:ns *ns*, :name (list 'quote (with-meta name-symb nil))})]
    `(def ~name-symb
       (let [impl# (impl/standard-multifn-impl ~combo ~dispatcher ~method-table)]
         (vary-meta (impl/multifn impl# ~mta ~cache) merge (meta (var ~name-symb)))))))

(defn ^:no-doc update-multimethod-metadata!
  "Part of the implementation for [[defmulti]]. Don't call this directly."
  [varr new-metadata]
  (let [new-doc (u/docstring-with-describe-output-appended (:doc new-metadata) (var-get varr))]
    (letfn [(merge-metadata [old-metadata]
              (merge (select-keys old-metadata [:ns :name :file :line :column])
                     new-metadata
                     {:original-doc (:doc new-metadata)
                      :doc          new-doc}))]
      (alter-meta! varr merge-metadata)
      ;; update the metadata on the multifn itself too.
      (alter-var-root varr (fn [multifn]
                             (vary-meta multifn merge-metadata)))))
  varr)

(defmacro defmulti
  "Creates a new Methodical multimethod named by a Var. Usage of this macro mimics usage of [[clojure.core/defmulti]],
  and it can be used as a drop-in replacement; it does, however, support a larger set of options. Note the dispatch-fn
  is optional (if omitted, then identity will be used). In addition to the usual `:default` and `:hierarchy` options,
  you many specify:

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
  have several alternative implementations available in [[methodical.impl]]. Defining additional implementations is
  straightforward as well: see [[methodical.interface]] for more details.

  Other improvements over [[clojure.core/defmulti]]:

  * Evaluating the form a second time (e.g., when reloading a namespace) will *not* redefine the multimethod, unless
    you have modified its form -- unlike vanilla Clojure multimethods, which need to be unmapped from the namespace to
    make such minor tweaks as changing the dispatch function.

  Attribute map options:

  `defmulti` supports a few additional options in its attributes map that will be used to validate `defmethod` forms
  during macroexpansion time. These are meant to help the users of your multimethods use them correctly by catching
  mistakes right away rather than waiting for them to pull their hair out later wondering why a method they added isn't
  getting called.

  * `:dispatch-value-spec` -- a spec for the `defmethod` dispatch value:

    ```clj
    (m/defmulti mf
      {:arglists '([x y]), :dispatch-value-spec (s/cat :x keyword?, :y int?)}
      (fn [x y] [x y]))

    (m/defmethod mf [:x 1]
      [x y]
      {:x x, :y y})
    ;; => ok

    (m/defmethod mf [:x]
      [x y]
      {:x x, :y y})
    ;; failed: Insufficient input in: [0] at: [:args-for-method-type :primary :dispatch-value :y] [:x]
    ```

    Note that this spec is applied to the unevaluated arguments at macroexpansion time, not the actual evaluated values.
    Note also that if you want to allow a `:default` method your spec will have to support it.

  * `:defmethod-arities` -- a set of allowed/required arities that `defmethod` forms are allowed to have. `defmethod`
    forms must have arities that match *all* of the specified `:defmethod-arities`, and all of its arities must be
    allowed by `:defmethod-arities`:

    ```clj
    (m/defmulti ^:private mf
      {:arglists '([x]), :defmethod-arities #{1}}
      keyword)

    (m/defmethod mf :x [x] x)
    ;; => ok

    (m/defmethod mf :x ([x] x) ([x y] x y))
    ;; => error: {:arities {:disallowed #{2}}}

    (m/defmethod mf :x [x y] x y)
    ;; => error: {:arities {:required #{1}}}


    (m/defmethod mf :x [x y] x)
    ;; => error: {:arities {:required #{1 [:>= 3]}, :disallowed #{2}}}
    ```

    `:defmethod-arities` must be a set of either integers or `[:> n]` forms to represent arities with `&` rest
    arguments, e.g. `[:>= 3]` to mean an arity of three *or-more* arguments:

    ```clj
    ;; methods must both a 1-arity and a 3+-arity
    (m/defmulti ^:private mf
      {:arglists '([x] [x y z & more]), :defmethod-arities #{1 [:>= 3]}}
      keyword)

    (m/defmethod mf :x ([x] x) ([x y z & more] x))
    ;; => ok
    ```

    When rest-argument arities are used, Methodical is smart enough to allow them when appropriate even if they do not
    specifically match an arity specified in `:defmethod-arities`:

    ```clj
    (m/defmulti ^:private mf
      {:arglists '([x y z & more]), :defmethod-arities #{[:>= 3]}}
      keyword)

    (m/defmethod mf :x
      ([a b c] x)
      ([a b c d] x)
      ([a b c d & more] x))
    ;; => ok, because everything required by [:>= 3] is covered, and everything present is allowed by [:>= 3]
    ```"
  {:arglists     '([name-symb docstring? attr-map? dispatch-fn?
                    & {:keys [hierarchy default-value prefers combo method-table cache]}]
                   [name-symb docstring? attr-map? & {:keys [dispatcher combo method-table cache]}])
   :style/indent :defn}
  [name-symb & args]
  (let [varr                                             (ns-resolve *ns* name-symb)
        {:keys [docstring attr-map dispatch-fn options]} (s/conform ::defmulti-args (cons name-symb args))
        options                                          (into {} (map (juxt :k :v)) options)
        metadata                                         (merge {:tag methodical.impl.standard.StandardMultiFn}
                                                                (when docstring {:doc docstring})
                                                                attr-map)
        old-val                                          (some->> varr deref (instance? StandardMultiFn))
        old-hash                                         (when old-val
                                                           (-> varr meta ::defmulti-hash))
        ;; hash should not include any metadata... do not redefine a multimethod if only metadata changes
        current-hash                                     (hash {:dispatch-fn dispatch-fn
                                                                :options     options})
        metadata                                         (assoc metadata ::defmulti-hash current-hash)
        name-symb                                        (vary-meta name-symb merge metadata)]
    ;; hashes and the like are expanded out into the macro to make what's going on more obvious when you expand it
    `(let [skip-redef?# (and
                         (let [~'old-hash     ~old-hash
                               ~'current-hash ~current-hash]
                           (= ~'old-hash ~'current-hash))
                         (some-> (ns-resolve *ns* '~name-symb) deref u/multifn?))]
       (if-not skip-redef?#
         (redefine-multimethod ~name-symb ~dispatch-fn ~options)
         (update-multimethod-metadata! (var ~name-symb) ~(merge (meta name-symb) metadata))))))

(s/fdef defmulti
  :args ::defmulti-args
  :ret  any?)

;;;; [[defmethod]]

(defn- default-dispatch-value-spec
  "A dispatch value as parsed to [[defmethod]] (i.e., not-yet-evaluated) can be ANYTHING other than the following two
  things:

  1. A legal aux qualifier for the current method combination, e.g. `:after` or `:around`

     It makes the parse for

     ```clj
     (m/defmethod mf :after \"str\" [_])
     ```

     ambiguous -- Is this an `:after` aux method with dispatch value `\"str\"`, or a primary method with dispatch value
     `:after` and a docstring? Since there's no clear way to decide which is which, we're going to have to disallow this.
     It's probably a good thing anyway since you're absolutely going to confuse the hell out of people if you use something
     like `:before` or `:around` as a *dispatch value*.

  2. A list that can be interpreted as part of a n-arity fn tail i.e. `([args ...] body ...)`

     I know, theoretically it should be possible to do something dumb like this:

     ```clj
     (doseq [i    [0 1]
             :let [toucan :toucan pigeon :pigeon]]
       (m/defmethod my-multimethod :before ([toucan pigeon] i)
         ([x]
          ...)))
     ```

     but we are just UNFORTUNATELY going to have to throw up our hands and say we don't support it. The reason is in
     the example above it's ambiguous whether this is a `:before` aux method with dispatch value `([toucan pigeon] i)`,
     or a primary method with dispatch value `:before`. It's just impossible to tell what you meant. If you really want
     to do something wacky like this, let-bind the dispatch value to a symbol or something.

  Note that if you specify a custom `:dispatch-value-spec` it overrides this spec. Hopefully your spec is stricter than
  this one is and it won't be a problem."
  [allowed-aux-qualifiers]
  (fn valid-dispatch-value? [x]
    (and (not (contains? allowed-aux-qualifiers x))
         (or (not (seq? x))
             (not (s/valid? :clojure.core.specs.alpha/params+body x))))))

(defn- dispatch-val-name
  "Generate a name based on a dispatch value. Used by [[method-fn-symbol]] below."
  [dispatch-val]
  (let [s (cond
            (sequential? dispatch-val)
            (str/join "-" (map dispatch-val-name dispatch-val))

            (and (instance? clojure.lang.Named dispatch-val)
                 (namespace dispatch-val))
            (str (namespace dispatch-val) "-" (name dispatch-val))

            (instance? clojure.lang.Named dispatch-val)
            (name dispatch-val)

            :else
            (munge (str dispatch-val)))]
    (-> s
        (str/replace #"\s+" "-")
        (str/replace  #"\." "-"))))

(defn- method-fn-symbol
  "Generate a nice name for a primary or auxiliary method's implementing function. Named functions are used rather than
  anonymous functions primarily to aid in debugging and improve stacktraces."
  ([multifn qualifier dispatch-val]
   (method-fn-symbol multifn qualifier dispatch-val nil))

  ([multifn qualifier dispatch-val unique-key]
   (let [s (cond-> (format "%s-%s-method-%s" (name multifn) (name qualifier) (dispatch-val-name dispatch-val))
             unique-key
             (str "-" unique-key))]
     (vary-meta (symbol s) assoc :private true, :multifn (when-let [ns-symb (some-> (:ns (meta multifn)) ns-name name)]
                                                           (when-let [symb (some-> (:name (meta multifn)) name)]
                                                             (list 'var (symbol ns-symb symb))))))))

(defn- emit-primary-method
  "Impl for [[defmethod]] for primary methods."
  [multifn {:keys [multifn-symb dispatch-value docstring fn-tail]}]
  (let [fn-symb (method-fn-symbol multifn "primary" dispatch-value)]
    `(do
       (defn ~fn-symb
         ~@(when docstring
             [docstring])
         ~@(i/transform-fn-tail multifn nil fn-tail))
       (u/add-primary-method! (var ~multifn-symb) ~dispatch-value (vary-meta ~fn-symb merge (meta (var ~fn-symb)))))))

(defn- emit-aux-method
  "Impl for [[defmethod]] for aux methods."
  [multifn {:keys [multifn-symb qualifier dispatch-value unique-key docstring fn-tail]}]
  (let [fn-symb    (method-fn-symbol multifn qualifier dispatch-value unique-key)
        unique-key (or unique-key (list 'quote (ns-name *ns*)))]
    `(do
       (defn ~fn-symb
         ~@(when docstring
             [docstring])
         ~@(i/transform-fn-tail multifn qualifier fn-tail))
       (u/add-aux-method-with-unique-key! (var ~multifn-symb)
                                          ~qualifier
                                          ~dispatch-value
                                          (vary-meta ~fn-symb merge (meta (var ~fn-symb)))
                                          ~unique-key))))

(defn- defmethod-args-spec [multifn]
  (let [allowed-qualifiers       (i/allowed-qualifiers multifn)
        primary-methods-allowed? (contains? allowed-qualifiers nil)
        allowed-aux-qualifiers   (disj allowed-qualifiers nil)
        dispatch-value-spec      (or (some-> (get (meta multifn) :dispatch-value-spec) s/spec)
                                     (default-dispatch-value-spec allowed-aux-qualifiers))
        arities-spec             (validate-arities/allowed-arities-fn-tail-spec (:defmethod-arities (meta multifn)))]
    (s/cat :args-for-method-type (s/alt :primary (if primary-methods-allowed?
                                                   (s/cat :dispatch-value dispatch-value-spec)
                                                   (constantly false))
                                        :aux     (s/cat :qualifier      allowed-aux-qualifiers
                                                        :dispatch-value dispatch-value-spec
                                                        :unique-key     (s/? (complement (some-fn string? sequential?)))))
           :docstring            (s/? string?)
           :fn-tail              (s/& (s/+ any?)
                                      (s/nonconforming (s/& ::fn-tail arities-spec))))))

(defn- parse-defmethod-args [multifn args]
  (let [spec      (defmethod-args-spec multifn)
        conformed (s/conform spec args)]
    (if (s/invalid? conformed)
      (throw (ex-info (s/explain-str spec args) (s/explain-data spec args)))
      (let [{[method-type type-args] :args-for-method-type} conformed]
        (-> (merge conformed {:method-type method-type} type-args)
            (dissoc :args-for-method-type))))))

(defn- resolve-multifn [multifn-symb]
  (doto (some-> (resolve multifn-symb) deref)
    (assert (format "Could not resolve multifn %s" multifn-symb))))

(defmacro defmethod
  "Define a new multimethod method implementation. Syntax is the same as for vanilla Clojure [[defmethod]], but you may
  also define auxiliary methods by passing an optional auxiliary method qualifier before the dispatch value:

  ```clj
  ;; define a new primary method
  (defmethod some-multifn Bird
  [_]
  ...)

  ;; define a new *auxiliary* method
  (defmethod some-multifn :before Toucan
  [_]
  ...)
  ```"
  {:arglists     '([multifn-symb dispatch-val docstring? & fn-tail]
                   [multifn-symb aux-qualifier dispatch-val unique-key? docstring? & fn-tail])
   :style/indent :defn}
  [multifn-symb & args]
  (let [multifn     (resolve-multifn multifn-symb)
        parsed-args (assoc (parse-defmethod-args multifn args) :multifn multifn, :multifn-symb multifn-symb)]
    ((case (:method-type parsed-args)
       :aux     emit-aux-method
       :primary emit-primary-method) multifn parsed-args)))

(s/fdef defmethod
  :args (s/& (s/cat :multifn-symb (every-pred symbol? resolve)
                    :args         (s/+ any?))
             ;; not sure if there's an easier way to do this.
             (fn [{:keys [multifn-symb args]}]
               (let [multifn   (resolve-multifn multifn-symb)
                     spec      (defmethod-args-spec multifn)
                     conformed (s/conform spec args)]
                 (when (s/invalid? conformed)
                   (throw (ex-info (s/explain-str spec args) (s/explain-data spec args))))
                 true)))
  :ret any?)
