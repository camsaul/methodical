(ns methodical.impl.method-table.common)

(defn- datafy-method [f]
  (let [mta (meta f)]
    (cond-> mta
      (:ns mta)
      (update :ns ns-name)

      (and (:name mta)
           (:ns mta))
      (update :name (fn [fn-name]
                      (symbol (str (ns-name (:ns mta))) (str fn-name))))

      true
      (dissoc :dispatch-value :private) ; we already know dispatch value. Whether it's private is irrelevant
      )))

(defn datafy-primary-methods
  "Helper for datafying a map of dispatch value -> method."
  [dispatch-value->fn]
  (into {}
        (map (fn [[dispatch-value f]]
               [dispatch-value (datafy-method f)]))
        dispatch-value->fn))

(defn- datafy-methods [fns]
  (mapv datafy-method fns))

(defn datafy-aux-methods
  "Helper for datafying a map of qualifier -> dispatch value -> methods."
  [qualifier->dispatch-value->fns]
  (into {}
        (map (fn [[qualifier dispatch-value->fns]]
               [qualifier (into {}
                                (map (fn [[dispatch-value fns]]
                                       [dispatch-value (datafy-methods fns)]))
                                dispatch-value->fns)]))
        qualifier->dispatch-value->fns))
