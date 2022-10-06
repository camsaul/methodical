(ns methodical.util
  "Utility functions for performing additional operations on multifns and their components not specified in one of the
  interfaces. These functions are compositions of those methods."
  (:refer-clojure :exclude [prefers prefer-method remove-all-methods])
  (:require
   [methodical.impl.standard :as impl.standard]
   [methodical.interface :as i]
   [methodical.util.describe :as describe]))

(set! *warn-on-reflection* true)

(defn multifn?
  "True if `x` is a Methodical multifn (i.e., if it is an instance of `StandardMultiFn`)."
  [x]
  (impl.standard/multifn? x))

(defn primary-method
  "Get the primary method *explicitly specified* for `dispatch-value`. This function does not return methods that would
  otherwise still be applicable (e.g., methods for ancestor dispatch values) -- just the methods explicitly defined
  for this exact match. (If you want methods that will be used, including those of ancestors dispatch values, you can
  use [[applicable-primary-method]] or [[effective-primary-method]] instead.)

  Note that the primary method will not have any implicit args (e.g. `next-method`) bound the way it normally would
  when combined into an effective method; you will need to supply this yourself (or pass `nil` for no `next-method`)."
  [multifn dispatch-val]
  (get (i/primary-methods multifn) dispatch-val))

(defn matching-primary-methods
  "Return a sequence of applicable primary methods for `dispatch-value`, sorted from most-specific to least-specific.
  Methods include the `^:dispatch-value` with which they were defined as metadata. The standard dispatcher also checks
  to make sure methods in the sequence are not ambiguously specific, replacing ambiguous methods with ones that will
  throw an Exception when invoked."
  ([multifn dispatch-val]
   (i/matching-primary-methods multifn multifn dispatch-val))
  ([dispatcher method-table dispatch-val]
   (i/matching-primary-methods dispatcher method-table dispatch-val)))

(defn applicable-primary-method
  "Return the primary method that would be use for `dispatch-value`, including ones from ancestor dispatch values or the
  default dipsatch value. Method includes `^:dispatch-value` metadata indicating the actual dispatch value for which
  the applicable method was defined.

  Like [[primary-method]], the method returned will not have any implicit args (such as `next-method`) bound."
  [multifn dispatch-val]
  (first (matching-primary-methods multifn dispatch-val)))

(defn effective-primary-method
  "Build and effective method equivalent that would be used for this `dispatch-value` if it had no applicable auxiliary
  methods. Implicit args (such as `next-method`) will be bound appropriately. Method has `^:dispatch-value` metadata
  for the dispatch value with which the most-specific primary method was defined."
  [multifn dispatch-val]
  (let [[most-specific-primary-method :as primary-methods] (matching-primary-methods multifn dispatch-val)]
    (some-> (i/combine-methods multifn primary-methods nil)
            (with-meta (meta most-specific-primary-method)))))

(defn aux-methods
  "Get all auxiliary methods *explicitly specified* for `dispatch-value`. This function does not include methods that
  would otherwise still be applicable (e.g., methods for ancestor dispatch values) -- the methods explicitly defined
  for this exact match.

   *  With 1 arg: methods come back as a map of `qualifier` -> `dispatch value` -> `[method]`.
   *  With 2 args: methods come back as a map of `qualifier` -> `[method]`.
   *  With 3 args: methods come back as sequence of `methods`."
  ([multifn]
   (i/aux-methods multifn))

  ([multifn dispatch-val]
   (let [qualifier->dispatch-val->fns (i/aux-methods multifn)]
     (when (seq qualifier->dispatch-val->fns)
       (into {} (for [[qualifier dispatch-val->fns] qualifier->dispatch-val->fns
                      :let                          [fns (get dispatch-val->fns dispatch-val)]
                      :when                         (seq fns)]
                  [qualifier fns])))))

  ([multifn qualifier dispatch-val]
   (get-in (i/aux-methods multifn) [qualifier dispatch-val])))

(defn matching-aux-methods
  "Return a map of aux method qualifier -> sequence of applicable methods for `dispatch-value`, sorted from
  most-specific to least-specific. Methods should have the `^:dispatch-value` with which they were defined as
  metadata."
  ([multifn dispatch-val]
   (i/matching-aux-methods multifn multifn dispatch-val))
  ([dispatcher method-table dispatch-val]
   (i/matching-aux-methods dispatcher method-table dispatch-val)))

(defn default-primary-method
  "Get the default primary method associated with this `mutlifn`, if one exists."
  [multifn]
  (primary-method multifn (i/default-dispatch-value multifn)))

(defn default-aux-methods
  "Get a map of aux qualifier -> methods for the default dispatch value, if any exist."
  [multifn]
  (aux-methods multifn (i/default-dispatch-value multifn)))

(defn default-effective-method
  "Return the effective (combined) method for the default dispatch value, if one can be computed."
  [multifn]
  (i/effective-method multifn (i/default-dispatch-value multifn)))

(defn effective-dispatch-value
  "Return the least-specific dispatch value that would return the same effective method as `dispatch-value`. e.g. if
  `dispatch-value` is `Integer` and the effective method is a result of combining a `Object` primary method and a
  `Number` aux method, the effective dispatch value is `Number`, since `Number` is the most specific thing out of the
  primary and aux methods and would get the same effective method as `Integer`."
  [multifn dispatch-val]
  (:dispatch-value (meta (i/effective-method multifn dispatch-val))))

(defn dispatch-value
  "Calculate the dispatch value that `multifn` will use given `args`."
  ;; since protocols can't define varargs, we have to wrap the `dispatch-value` method from the protocol and apply
  ;; varargs for > 4 args. The various < 4 args arities are there as an optimization because it's a little faster than
  ;; calling apply every time.
  ([multifn a]              (i/dispatch-value multifn a))
  ([multifn a b]            (i/dispatch-value multifn a b))
  ([multifn a b c]          (i/dispatch-value multifn a b c))
  ([multifn a b c d]        (i/dispatch-value multifn a b c d))
  ([multifn a b c d & more] (i/dispatch-value multifn a b c d more)))

(defn dispatch-fn
  "Return a function that can be used to calculate dispatch values of given arg(s)."
  [multifn]
  (partial dispatch-value multifn))

(defn remove-all-primary-methods
  "Remove all primary methods, for all dispatch values (including the default value), for this `multifn` or method
  table."
  [multifn]
  (reduce
   i/remove-primary-method
   multifn
   (keys (i/primary-methods multifn))))

(defn remove-all-aux-methods
  "With one arg, remove *all* auxiliary methods for a `multifn`. With two args, remove all auxiliary methods for the
  given `qualifier` (e.g. `:before`). With three args, remove all auxiliary methods for a given `qualifier` and
  `dispatch-value`."
  ([multifn]
   (reduce remove-all-aux-methods multifn (keys (i/aux-methods multifn))))

  ([multifn qualifier]
   (reduce
    (fn [multifn dispatch-val]
      (remove-all-aux-methods multifn qualifier dispatch-val))
    multifn
    (keys (get (i/aux-methods multifn) qualifier))))

  ([multifn qualifier dispatch-val]
   (reduce
    (fn [multifn f]
      (i/remove-aux-method multifn qualifier dispatch-val f))
    multifn
    (get-in (i/aux-methods multifn) [qualifier dispatch-val]))))

;; TODO -- consider renaming to `remove-all-aux-methods-for-dispatch-val` for consistency with everything else
(defn remove-all-aux-methods-for-dispatch-val
  "Remove all auxiliary methods for `dispatch-value` for *all* qualifiers."
  [multifn dispatch-val]
  (reduce
   (fn [multifn qualifier]
     (remove-all-aux-methods multifn qualifier dispatch-val))
   multifn
   (keys (i/aux-methods multifn))))

(defn remove-aux-method-with-unique-key
  "Remove an auxiliary method that was added by [[add-aux-method-with-unique-key]], if one exists. Returns `multifn`."
  [multifn qualifier dispatch-val unique-key]
  {:pre [(some? multifn)]}
  (if-let [method (some
                   (fn [method]
                     (when (= (:methodical/unique-key (meta method)) unique-key)
                       method))
                   (aux-methods multifn qualifier dispatch-val))]
    (i/remove-aux-method multifn qualifier dispatch-val method)
    multifn))

(defn add-aux-method-with-unique-key
  "Adds an auxiliary method with a `unique-key` stored in its metadata. This unique key can later be used to remove the
  auxiliary method with [[remove-aux-method-with-unique-key]]. If a method with this key already exists for this
  qualifier and dispatch value, replaces the original."
  [multifn qualifier dispatch-val f unique-key]
  {:pre [(some? multifn)]}
  (-> multifn
      (remove-aux-method-with-unique-key qualifier dispatch-val unique-key)
      (i/add-aux-method qualifier dispatch-val (vary-meta f assoc :methodical/unique-key unique-key))))

(defn remove-all-methods
  "Remove all primary and auxiliary methods, including default implementations."
  [multifn]
  (-> multifn remove-all-primary-methods remove-all-aux-methods))

(defn add-preference
  "Add a method preference to `prefs` for dispatch value `x` over `y`. Used to implement [[prefer-method]]. `isa?*` is
  used to determine whether a relationship between `x` and `y` that precludes this preference already exists; it can
  be [[clojure.core/isa?]], perhaps partially bound with a hierarchy, or some other 2-arg predicate function."
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

(defn prefer-method
  "Prefer `dispatch-val-x` over `dispatch-val-y` for dispatch and method combinations. You can undo this preference
  with [[unprefer-method]]."
  [multifn dispatch-val-x dispatch-val-y]
  {:pre [(some? multifn)]}
  (when (= dispatch-val-x dispatch-val-y)
    (throw (IllegalStateException. (format "Cannot prefer dispatch value %s over itself." dispatch-val-x))))
  (let [prefs (i/prefers multifn)]
    (when (contains? (get prefs dispatch-val-y) dispatch-val-x)
      (throw (IllegalStateException. (format "Preference conflict in multimethod: %s is already preferred to %s"
                                             dispatch-val-y
                                             dispatch-val-x))))
    (when (i/dominates? (i/with-prefers multifn nil) dispatch-val-y dispatch-val-x)
      (throw (IllegalStateException.
              (format "Preference conflict in multimethod: cannot prefer %s over its descendant %s."
                      dispatch-val-x
                      dispatch-val-y))))
    (let [new-prefs (update prefs dispatch-val-x #(conj (set %) dispatch-val-y))]
      (i/with-prefers multifn new-prefs))))

(defn- remove-preference [preferences dispatch-value-x dispatch-value-y]
  (let [updated-preferences (update preferences dispatch-value-x (fn [x-preferences]
                                                                   (disj (set x-preferences) dispatch-value-y)))]
    (if (empty? (get updated-preferences dispatch-value-x))
      (dissoc updated-preferences dispatch-value-x)
      updated-preferences)))

(defn unprefer-method
  "Return a copy of `multifn` with any preferences of `dispatch-val-x` over `dispatch-val-y` removed. If no such
  preference exists, this returns `multifn` as-is. Opposite of [[prefer-method]].

  To destructively remove a dispatch value preference, use [[unprefer-method!]]."
  [multifn dispatch-val-x dispatch-val-y]
  {:pre [(some? multifn)]}
  (let [preferences         (i/prefers multifn)
        updated-preferences (remove-preference preferences dispatch-val-x dispatch-val-y)]
    (if (= preferences updated-preferences)
      ;; return multifn as is if nothing has changed.
      multifn
      (i/with-prefers multifn updated-preferences))))

(defn remove-all-preferences
  "Return a copy of `multifn` with all of its preferences for all dispatch values removed.

  To destructively remove all preferences, use [[remove-all-preferences!]]."
  [multifn]
  {:pre [(some? multifn)]}
  (if (empty? (i/prefers multifn))
    multifn
    (i/with-prefers multifn {})))

(defn is-default-effective-method?
  "When `multifn` is invoked with args that have `dispatch-val`, will we end up using the default effective
  method (assuming one exists)?"
  [multifn dispatch-val]
  ;; we need to make sure that a default method is present before calculating this stuff,
  ;; otherwise [[i/effective-method]] and [[default-effective-method]] will both return `nil`, giving us a false
  ;; positive here, even if there is an applicable non-default aux method. Also we need to make sure `{:dispatch-value
  ;; nil}` doesn't get confused with `nil` because there is no matching default method.
  (let [multifn (i/add-primary-method multifn (i/default-dispatch-value multifn) (constantly nil))]
    (= (:dispatch-value (meta (i/effective-method multifn dispatch-val)))
       (:dispatch-value (meta (default-effective-method multifn))))))

(defn is-default-primary-method?
  "When `multifn` is invoked with args that have `dispatch-val`, will we end up using the default primary method (assuming
  one exists)?"
  [multifn dispatch-val]
  ;; We need to make sure `{:dispatch-value nil}` for the effective primary method doesn't get confused with `nil`
  ;; if `(default-primary-method multifn)` doesn't return anything because there is no default method.
  (let [multifn (i/add-primary-method multifn (i/default-dispatch-value multifn) (constantly nil))]
    (= (:dispatch-value (meta (effective-primary-method multifn dispatch-val)))
       (:dispatch-value (meta (default-primary-method multifn))))))


;;;; #### Low-level destructive operations

(defn ^:no-doc docstring-with-describe-output-appended
  "Build a docstring by taking the original user-supplied `:doc` and the output of [[describe/describe]]."
  (^String [varr]
   (let [original-doc  ((some-fn :original-doc :doc) (meta varr))
         updated-value (var-get varr)]
     (docstring-with-describe-output-appended original-doc updated-value)))

  (^String [original-doc updated-value]
   (str
    (when (seq original-doc)
      (str original-doc \newline \newline))
    (describe/describe updated-value))))

(defn alter-var-root+
  "Like [[clojure.core/alter-var-root]], but handles vars that are aliases of other vars, e.g. ones that have been
  imported via Potemkin [[potemkin/import-vars]]."
  [multifn-var f & args]
  (let [{var-ns :ns, var-name :name} (meta multifn-var)
        varr                         (if (and var-ns var-name)
                                       (ns-resolve var-ns var-name)
                                       multifn-var)
        original-doc                 ((some-fn :original-doc :doc) (meta multifn-var))]
    (apply alter-var-root varr f args)
    (let [new-doc (docstring-with-describe-output-appended varr)]
      (alter-meta! multifn-var assoc :original-doc original-doc, :doc new-doc))
    multifn-var))

(defn add-primary-method!
  "Destructive version of [[add-primary-method]]. Operates on a var defining a Methodical multifn."
  [multifn-var dispatch-val f]
  (alter-var-root+ multifn-var i/add-primary-method dispatch-val f))

(defn remove-primary-method!
  "Destructive version of [[methodical.interface/remove-primary-method]]. Operates on a var defining a Methodical multifn."
  [multifn-var dispatch-val]
  (alter-var-root+ multifn-var i/remove-primary-method dispatch-val))

(defn remove-all-primary-methods!
  "Destructive version of [[remove-all-primary-methods]]. Operates on a var defining a Methodical multifn."
  [multifn-var]
  (alter-var-root+ multifn-var remove-all-primary-methods))

(defn add-aux-method!
  "Destructive version of [[methodical.interface/add-aux-method]]. Operates on a var defining a Methodical multifn."
  [multifn-var qualifier dispatch-val f]
  (alter-var-root+ multifn-var i/add-aux-method qualifier dispatch-val f))

(defn remove-aux-method!
  "Destructive version of [[methodical.interface/remove-aux-method]]. Operates on a var defining a Methodical multifn."
  [multifn-var qualifier dispatch-val f]
  (alter-var-root+ multifn-var i/remove-aux-method qualifier dispatch-val f))

(defn remove-all-aux-methods!
  "Destructive version of [[remove-all-aux-methods]]. Operates on a var defining a Methodical multifn."
  ([multifn-var]
   (alter-var-root+ multifn-var remove-all-aux-methods))

  ([multifn-var qualifier]
   (alter-var-root+ multifn-var remove-all-aux-methods qualifier))

  ([multifn-var qualifier dispatch-val]
   (alter-var-root+ multifn-var remove-all-aux-methods qualifier dispatch-val)))

(defn remove-all-aux-methods-for-dispatch-val!
  "Destructive version of [[remove-all-aux-methods-for-dispatch-val]]. Operates on a var defining a Methodical multifn."
  [multifn-var dispatch-val]
  (alter-var-root+ multifn-var remove-all-aux-methods-for-dispatch-val dispatch-val))

(defn add-aux-method-with-unique-key!
  "Destructive version of [[add-aux-method-with-unique-key]]. Operates on a var defining a Methodical multifn."
  [multifn-var qualifier dispatch-val f unique-key]
  (alter-var-root+ multifn-var add-aux-method-with-unique-key qualifier dispatch-val f unique-key))

(defn remove-aux-method-with-unique-key!
  "Destructive version of [[remove-aux-method-with-unique-key]]. Operates on a var defining a Methodical multifn."
  [multifn-var qualifier dispatch-val unique-key]
  (alter-var-root+ multifn-var remove-aux-method-with-unique-key qualifier dispatch-val unique-key))

(defn remove-all-methods!
  "Destructive version of [[remove-all-methods]]. Operates on a var defining a Methodical multifn."
  [multifn-var]
  (alter-var-root+ multifn-var remove-all-methods))

(defn with-prefers!
  "Destructive version of [[methodical.interface/with-prefers]]. Operates on a var defining a Methodical multifn."
  [multifn-var new-prefs]
  (alter-var-root+ multifn-var i/with-prefers new-prefs))

(defn prefer-method!
  "Destructive version of [[prefer-method]]. Operates on a var defining a Methodical multifn.

  Note that vanilla Clojure [[clojure.core/prefer-method]] is actually itself destructive, so this function is
  actually the Methodical equivalent of that function. `prefer-method!` is used by Methodical to differentiate the
  operation from our nondestructive [[prefer-method]], which returns a copy of the multifn with an altered dispatch
  table."
  [multifn-var dispatch-val-x dispatch-val-y]
  (alter-var-root+ multifn-var prefer-method dispatch-val-x dispatch-val-y))

(defn unprefer-method!
  "Destructive version of [[unprefer-method]]. Operates on a var defining a Methodical multifn."
  [multifn-var dispatch-val-x dispatch-val-y]
  (alter-var-root+ multifn-var unprefer-method dispatch-val-x dispatch-val-y))

(defn remove-all-preferences!
  "Destructive version of [[remove-all-preferences]]. Operates on a var defining a Methodical multifn."
  [multifn-var]
  (alter-var-root+ multifn-var remove-all-preferences))
