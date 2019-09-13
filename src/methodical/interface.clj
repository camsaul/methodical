(ns methodical.interface
  (:refer-clojure :exclude [isa? prefers prefer-method]))

(defmacro ^:private defonceinterface [name & body]
  (let [class-name (clojure.string/replace (str *ns* "." name) #"\-" "_")
        exists     (try
                     (Class/forName class-name)
                     true
                     (catch Exception _
                       false))]
    (if exists
      `(do
         (import ~(symbol class-name))
         nil)
      `(definterface ~name ~@body))))

(defonceinterface MethodCombination
  (allowedQualifiers [])
  (combineMethods [primary-methods aux-methods])
  (transformFnTail [qualifier fn-tail]))

(defn allowed-qualifiers
  "The set containg all qualifiers supported by this method combination. `nil` in the set means the method
    combination supports primary methods (because primary methods have no qualifier); all other values refer to
    auxiliary methods with that qualifer, e.g. `:before`, `:after`, or `:around`.

    (allowed-qualifiers (clojure-method-combination)) ;-> #{nil}
    (allowed-qualifiers (clos-method-combination))    ;-> #{nil :before :after :around}
    (allowed-qualifiers (doseq-method-combination))   ;-> #{:doseq}"
  [^MethodCombination method-combination]
  (.allowedQualifiers method-combination))

(defn combine-methods
  "Combine a sequence of matching `primary-methods` with `aux-methods` (a map of qualifier -> sequence of methods)
    into a single effective method."
  [^MethodCombination method-combination primary-methods aux-methods]
  (.combineMethods method-combination primary-methods aux-methods))

(defn transform-fn-tail
  "Make appropriate transformations to the `fn-tail` of a `defmethod` macro expansion for a primary
    method (qualifier will be `nil`) or an auxiliary method. You can use this method to add implicit args like
    `next-method` to the body of a `defmethod` macro. (Because this method is invoked during macroexpansion, it should
    return a Clojure form.)"
  [^MethodCombination method-combination qualifier fn-tail]
  (.transformFnTail method-combination qualifier fn-tail))

(defonceinterface MethodTable
  (primaryMethods [])
  (auxMethods [])
  (addPrimaryMethod [dispatch-value f])
  (removePrimaryMethod [dispatch-value])
  (addAuxMethod [qualifier dispatch-value f])
  (removeAuxMethod [qualifier dispatch-val method]))

(defn primary-methods
  "Get a `dispatch-value -> fn` map of all primary methods assoicated with this method table."
  [^MethodTable method-table]
  (.primaryMethods method-table))

(defn aux-methods
  "Get a `qualifier -> dispatch-value -> [fn]` map of all auxiliary methods associated with this method table."
  [^MethodTable method-table]
  (.auxMethods method-table))

(defn add-primary-method
  "Set the primary method implementation for `dispatch-value`, replacing it if it already exists."
  [^MethodTable method-table dispatch-value f]
  (.addPrimaryMethod method-table dispatch-value f))

(defn remove-primary-method
  "Remove the primary method for `dispatch-value`."
  [^MethodTable method-table dispatch-value]
  (.removePrimaryMethod method-table dispatch-value))

(defn add-aux-method
  "Add an auxiliary method implementation for `qualifer` (e.g. `:before`) and `dispatch-value`. Unlike primary
    methods, auxiliary methods are not limited to one method per dispatch value; thus this method does not remove
    existing methods for this dispatch value. existing "
  [^MethodTable method-table qualifier dispatch-value f]
  (.addAuxMethod method-table qualifier dispatch-value f))

(defn remove-aux-method
  "Remove an auxiliary method from a method table. Because multiple auxiliary methods are allowed for the same
    dispatch value, existing implementations of `MethodTable` are currently only able to remove exact matches -- for
    functions, this usually means identical objects.

    In the future, I hope to fix this by storing unique indentifiers in the metadata of methods in the map."
  [^MethodTable method-table qualifier dispatch-val method]
  (.removeAuxMethod method-table qualifier dispatch-val method))

(defonceinterface Dispatcher
  (dispatchValue [])
  (dispatchValue [a])
  (dispatchValue [a b])
  (dispatchValue [a b c])
  (dispatchValue [a b c d])
  (dispatchValue [a b c d more])

  (matchingPrimaryMethods [method-table dispatch-value])
  (matchingAuxMethods [method-table dispatch-value])
  (defaultDispatchValue [])
  (prefers [])
  (preferMethod [dispatch-val-x dispatch-val-y]))

(defn dispatch-value
  "Return an appropriate dispatch value for args passed to a multimethod. (This method is equivalent in purpose to
    the dispatch function of vanilla Clojure multimethods.)"
  ([^Dispatcher dispatcher] (.dispatchValue dispatcher))
  ([^Dispatcher dispatcher a] (.dispatchValue dispatcher a))
  ([^Dispatcher dispatcher a b] (.dispatchValue dispatcher a b))
  ([^Dispatcher dispatcher a b c] (.dispatchValue dispatcher a b c))
  ([^Dispatcher dispatcher a b c d] (.dispatchValue dispatcher a b c d))
  ([^Dispatcher dispatcher a b c d more] (.dispatchValue dispatcher a b c d more)))

(defn matching-primary-methods
  "Return a sequence of applicable primary methods for `dispatch-value`, sorted from most-specific to
    least-specific. The standard dispatcher also checks to make sure methods in the sequence are not
    ambiguously specific, replacing ambiguous methods with ones that will throw an Exception when invoked."
  [^Dispatcher dispatcher method-table dispatch-value]
  (.matchingPrimaryMethods dispatcher method-table dispatch-value))

(defn matching-aux-methods
  "Return a map of aux method qualifier -> sequence of applicable methods for `dispatch-value`, sorted from
    most-specific to least-specific."
  [^Dispatcher dispatcher method-table dispatch-value]
  (.matchingAuxMethods dispatcher method-table dispatch-value))

(defn default-dispatch-value
 "Default dispatch value to use if no other dispatch value matches."
 [^Dispatcher dispatcher]
 (.defaultDispatchValue dispatcher))

(defn prefers
  "Return a map of preferred dispatch value -> set of other dispatch values."
  [^Dispatcher dispatcher]
  (.prefers dispatcher))

(defn prefer-method
  "Prefer `dispatch-val-x` over `dispatch-val-y` for dispatch and method combinations."
  [^Dispatcher dispatcher dispatch-val-x dispatch-val-y]
  (.preferMethod dispatcher dispatch-val-x dispatch-val-y))

(defonceinterface MultiFnImpl
  (^methodical.interface.MethodCombination methodCombination [])
  (^methodical.interface.Dispatcher dispatcher [])
  (^methodical.interface.MultiFnImpl withDispatcher [new-dispatcher])
  (^methodical.interface.MethodTable methodTable [])
  (^methodical.interface.MultiFnImpl withMethodTable [new-method-table])
  (effectiveMethod [dispatch-value]))

(defn ^methodical.interface.MethodCombination method-combination
  "Get the method combination associated with this multifn."
  [^MultiFnImpl multifn]
  (.methodCombination multifn))

(defn ^methodical.interface.Dispatcher dispatcher
  "Get the dispatcher associated with this multifn."
  [^MultiFnImpl multifn]
  (.dispatcher multifn))

(defn ^methodical.interface.MultiFnImpl with-dispatcher
  "Return a copy of this multifn using `new-dispatcher` as its dispatcher."
  [^MultiFnImpl multifn new-dispatcher]
  (.withDispatcher multifn new-dispatcher))

(defn ^methodical.interface.MethodTable method-table
  "Get the method table associated with this multifn."
  [^MultiFnImpl multifn]
  (.methodTable multifn))

(defn ^methodical.interface.MultiFnImpl with-method-table
  "Return a copy of this multifn using `new-method-table` as its method table."
  [^MultiFnImpl multifn new-method-table]
  (.withMethodTable multifn new-method-table))

(defn effective-method
  "Return the effective method for `dispatch-value`. The effective method is a combined primary method and
    applicable auxiliary methods that can be called like a normal function. `effective-method` is similar in purpose
    to `get-method` in vanilla Clojure multimethods; a different name is used here because I felt `get-method` would
    be ambiguous with regards to whether it returns only a primary method or a combined effective method."
  [^MultiFnImpl multifn dispatch-value]
  (.effectiveMethod multifn dispatch-value))

(defonceinterface Cache
  (cachedMethod [dispatch-value])
  (cacheMethodBang [dispatch-value method])
  (clearCacheBang [])
  (^methodical.interface.Cache emptyCopy []))

(defn cached-method
  "Return cached effective method for `dispatch-value`, if it exists in the cache."
  [^Cache cache dispatch-value]
  (.cachedMethod cache dispatch-value))

(defn cache-method!
  "Cache the effective method for `dispatch-value` in this cache."
  [^Cache cache dispatch-value method]
  (.cacheMethodBang cache dispatch-value method))

(defn clear-cache!
  "Empty the contents of the cache in-place."
  [^Cache cache]
  (.clearCacheBang cache))

(defn ^methodical.interface.Cache empty-copy
  "Return an empty copy of the same type as this cache, e.g. for use when copying a multifn."
  [^Cache cache]
  (.emptyCopy cache))
