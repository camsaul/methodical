(ns methodical.util
  "Utility functions for performing additional operations on multifns and their components not specified in one of the
  interfaces. These functions are compositions of those methods."
  (:refer-clojure :exclude [prefers prefer-method remove-all-methods])
  (:require [methodical.impl.standard :as impl.standard]
            [methodical.interface :as i]))

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
  [multifn dispatch-value]
  (get (i/primary-methods multifn) dispatch-value))

(defn applicable-primary-method
  "Return the primary method that would be use for `dispatch-value`, including ones from ancestor dispatch values or the
  default dipsatch value.

  Like `primary-method`, the method returned will not have any implicit args (such as `next-method`) bound."
  [multifn dispatch-value]
  (first (i/matching-primary-methods multifn multifn dispatch-value)))

(defn effective-primary-method
  "Build and effective method equivalent that would be used for this `dispatch-value` if it had no applicable auxiliary
  methods. Implicit args (such as `next-method`) will be bound appropriately."
  [multifn dispatch-value]
  (i/combine-methods multifn (i/matching-primary-methods multifn multifn dispatch-value) nil))

(defn aux-methods
  "Get all auxiliary methods *explicitly specified* for `dispatch-value`. This function does not include methods that
  would otherwise still be applicable (e.g., methods for ancestor dispatch values) -- the methods explicitly defined
  for this exact match.

   *  With 1 arg: methods come back as a map of `qualifier` -> `dispatch value` -> `[method]`.
   *  With 2 args: methods come back as a map of `qualifier` -> `[method]`.
   *  With 3 args: methods come back as sequence of `methods`."
  ([multifn]
   (i/aux-methods multifn))

  ([multifn dispatch-value]
   (let [qualifier->dispatch-value->fns (i/aux-methods multifn)]
     (when (seq qualifier->dispatch-value->fns)
       (into {} (for [[qualifier dispatch-value->fns] qualifier->dispatch-value->fns
                      :let                            [fns (get dispatch-value->fns dispatch-value)]
                      :when                           (seq fns)]
                  [qualifier fns])))))

  ([multifn qualifier dispatch-value]
   (get-in (i/aux-methods multifn) [qualifier dispatch-value])))

(defn default-primary-method
  "Get the default primary method associated with this `mutlifn`, if one exists."
  [multifn]
  (primary-method multifn (i/default-dispatch-value multifn)))

(defn default-aux-methods
  "Get a map of aux qualifer -> methods for the default dispatch value, if any exist."
  [multifn]
  (aux-methods multifn (i/default-dispatch-value multifn)))

(defn default-effective-method
  "Return the effective (combined) method for the default dispatch value, if one can be computed."
  [multifn]
  (i/effective-method multifn (i/default-dispatch-value multifn)))

(defn dispatch-fn
  "Return a function that can be used to calculate dispatch values of given arg(s)."
  [multifn]
  (partial i/dispatch-value multifn))

(defn remove-all-primary-methods
  "Remove all primary methods, for all dispatch values (including the default value), for this `multifn` or method
  table."
  [multifn]
  (reduce
   (fn [multifn dispatch-value]
     (i/remove-primary-method multifn dispatch-value))
   multifn
   (keys (i/primary-methods multifn))))

(defn remove-all-aux-methods
  "With one arg, remove *all* auxiliary methods for a `multifn`. With two args, remove all auxiliary methods for the
  given `qualifier` (e.g. `:before`). With three args, remove all auxiliary methods for a given `qualifier` and
  `dispatch-value`. "
  ([multifn]
   (reduce
    (fn [multifn qualifier]
      (remove-all-aux-methods multifn qualifier))
    multifn
    (keys (i/aux-methods multifn))))

  ([multifn qualifier]
   (reduce
    (fn [multifn dispatch-value]
      (remove-all-aux-methods multifn qualifier dispatch-value))
    multifn
    (keys (get (i/aux-methods multifn) qualifier))))

  ([multifn qualifier dispatch-value]
   (reduce
    (fn [multifn f]
      (i/remove-aux-method multifn qualifier dispatch-value f))
    multifn
    (get-in (i/aux-methods multifn) [qualifier dispatch-value]))))

(defn remove-all-aux-methods-for-dispatch-val
  "Remove all auxiliary methods for `dispatch-value` for *all* qualifiers."
  [multifn dispatch-value]
  (reduce
   (fn [multifn qualifier]
     (remove-all-aux-methods multifn qualifier dispatch-value))
   multifn
   (keys (i/aux-methods multifn))))

(defn remove-aux-method-with-unique-key
  "Remove an auxiliary method that was added by `add-aux-method-with-unique-key`, if one exists. Returns multifn."
  [multifn qualifier dispatch-val unique-key]
  {:pre [(some? multifn)]}
  (if-let [method (some
                   (fn [method]
                     (when (= (::unique-key (meta method)) unique-key)
                       method))
                   (aux-methods multifn qualifier dispatch-val))]
    (i/remove-aux-method multifn qualifier dispatch-val method)
    multifn))

(defn add-aux-method-with-unique-key
  "Adds an auxiliary method with a `unique-key` stored in its metadata. This unique key can later be used to remove the
  auxiliary method with `remove-aux-method-with-unique-key`. If a method with this key already exists for this
  qualifier and dispatch value, replaces the original."
  [multifn qualifier dispatch-val f unique-key]
  {:pre [(some? multifn)]}
  (-> multifn
      (remove-aux-method-with-unique-key qualifier dispatch-val unique-key)
      (i/add-aux-method qualifier dispatch-val (vary-meta f assoc ::unique-key unique-key))))

(defn remove-all-methods
  "Remove all primary and auxiliary methods, including default implementations."
  [multifn]
  (-> multifn remove-all-primary-methods remove-all-aux-methods))


;;;; #### Low-level destructive operations

(defn add-primary-method!
  "Destructive version of `add-primary-method`. Operates on a var defining a Methodical multifn."
  [multifn-var dispatch-val f]
  (alter-var-root multifn-var i/add-primary-method dispatch-val f))

(defn remove-primary-method!
  "Destructive version of `remove-primary-method`. Operates on a var defining a Methodical multifn."
  [multifn-var dispatch-val]
  (alter-var-root multifn-var i/remove-primary-method dispatch-val))

(defn remove-all-primary-methods!
  "Destructive version of `remove-all-primary-methods`. Operates on a var defining a Methodical multifn."
  [multifn-var]
  (alter-var-root multifn-var remove-all-primary-methods))

(defn add-aux-method!
  "Destructive version of `add-aux-method`. Operates on a var defining a Methodical multifn."
  [multifn-var qualifier dispatch-val f]
  (alter-var-root multifn-var i/add-aux-method qualifier dispatch-val f))

(defn remove-aux-method!
  "Destructive version of `remove-aux-method`. Operates on a var defining a Methodical multifn."
  [multifn-var qualifier dispatch-val f]
  (alter-var-root multifn-var i/remove-aux-method qualifier dispatch-val f))

(defn remove-all-aux-methods!
  "Destructive version of `remove-all-aux-methods`. Operates on a var defining a Methodical multifn."
  ([multifn-var]
   (alter-var-root multifn-var remove-all-aux-methods))

  ([multifn-var qualifier]
   (alter-var-root multifn-var remove-all-aux-methods qualifier))

  ([multifn-var qualifier dispatch-val]
   (alter-var-root multifn-var remove-all-aux-methods qualifier dispatch-val)))

(defn remove-all-aux-methods-for-dispatch-val!
  "Destructive version of `remove-all-aux-methods-for-dispatch-val`. Operates on a var defining a Methodical multifn."
  [multifn-var dispatch-value]
  (alter-var-root multifn-var remove-all-aux-methods-for-dispatch-val dispatch-value))

(defn add-aux-method-with-unique-key!
  "Destructive version of `add-aux-method-with-unique-key`. Operates on a var defining a Methodical multifn."
  [multifn-var qualifier dispatch-val f unique-key]
  (alter-var-root multifn-var add-aux-method-with-unique-key qualifier dispatch-val f unique-key))

(defn remove-aux-method-with-unique-key!
  "Destructive version of `remove-aux-method-with-unique-key`. Operates on a var defining a Methodical multifn."
  [multifn-var qualifier dispatch-val unique-key]
  (alter-var-root multifn-var remove-aux-method-with-unique-key qualifier dispatch-val unique-key))

(defn remove-all-methods!
  "Destructive version of `remove-all-methods`. Operates on a var defining a Methodical multifn."
  [multifn-var]
  (alter-var-root multifn-var remove-all-methods))

(defn prefer-method!
  "Destructive version of `prefer-method`. Operates on a var defining a Methodical multifn.

  Note that vanilla Clojure `prefer-method` is actually itself destructive, so this function is actually the
  Methodical equivalent of that function. `prefer-method!` is used by Methodical to differentiate the operation from
  our nondestructive `prefer-method`, which returns a copy of the multifn with an altered dispatch table."
  [multifn-var dispatch-val-x dispatch-val-y]
  (alter-var-root multifn-var i/prefer-method dispatch-val-x dispatch-val-y))
