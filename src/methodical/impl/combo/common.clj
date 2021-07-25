(ns methodical.impl.combo.common
  "Utility functions for implementing method combinations.")

(defn combine-primary-methods
  "Combine all `primary-methods` into a single combined method. Each method is partially bound with a `next-method`
  arg."
  [primary-methods]
  (when (seq primary-methods)
    (reduce
     (fn [next-method primary-method]
       (with-meta (partial primary-method next-method) (meta primary-method)))
     nil
     (reverse primary-methods))))

(defn apply-around-methods
  "Combine `around-methods` into `combined-method`, returning a new even-more-combined method. Each around method is
  partially bound with a `next-method` arg. Normally, this applies around methods least-specific-first (e.g. Person
  before Child)."
  [combined-method around-methods]
  (reduce
   (fn [combined-method around-method]
     (with-meta (partial around-method combined-method) (meta around-method)))
   combined-method
   around-methods))

;;;; #### Helpers for implementing `transform-fn-tail`

(defn transform-fn-tail
  "Transform `fn-tail` using f, a function that operates on a single `([params*] expr*)` form. For single-arity
  functions, this applies `f` directly to `fn-tail`; for functions overloaded with multiple arities, this maps `f`
  across all arities."
  [f fn-tail]
  {:pre [(sequential? fn-tail)]}
  (cond
    (vector? (first fn-tail))
    (apply f fn-tail)

    (vector? (ffirst fn-tail))
    (map (partial transform-fn-tail f) fn-tail)

    :else
    (throw (ex-info (format "Invalid fn tail: %s. Expected ([arg*] & body) or (([arg*] & body)+)"
                            (pr-str fn-tail))
                    {:f f, :fn-tail fn-tail}))))

(defn add-implicit-arg
  "Add an implicit `arg` to the beginning of the arglists for every arity of `fn-tail`."
  [arg fn-tail]
  (transform-fn-tail
   (fn [bindings & body]
     (cons (into [arg] bindings) body))
   fn-tail))

(defn add-implicit-next-method-args
  "Add an implicit `next-method` arg to the beginning of primary and `:around` fn tails; `:before` and `:after` tails
  are left as-is."
  [qualifier fn-tail]
  (case qualifier
    nil     (add-implicit-arg 'next-method fn-tail)
    :before fn-tail
    :after  fn-tail
    :around (add-implicit-arg 'next-method fn-tail)))
