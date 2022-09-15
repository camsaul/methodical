(ns methodical.impl.method-table.common
  (:require [clojure.string :as str]))

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

(defn- describe-method
  ([f]
   (let [{method-ns :ns, :keys [line file doc]} (meta f)]
     (str/join
      \space
      [(when method-ns
         (format "defined in [[%s]]" (ns-name method-ns)))
       (cond
         (and file line)
         (format "(%s:%d)" file line)

         file
         (format "(%s)" file))
       (when doc
         (format "\n\nIt has the following documentation:\n\n%s" doc))])))

  ([dispatch-value f]
   (format "* `%s`, %s" (pr-str dispatch-value) (str/join
                                                 "\n  "
                                                 (str/split-lines (describe-method f))))))

(defn describe-primary-methods
  "Helper for [[methodical.util.describe/describe]]ing the primary methods in a method table."
  ^String [dispatch-value->method]
  (when (seq dispatch-value->method)
    (format
     "\n\nThese primary methods are known:\n\n%s"
     (str/join
      "\n\n"
      (for [[dispatch-value f] dispatch-value->method]
        (describe-method dispatch-value f))))))

(defn describe-aux-methods
  "Helper for [[methodical.util.describe/describe]]ing the aux methods in a method table."
  ^String [qualifier->dispatch-value->methods]
  (when (seq qualifier->dispatch-value->methods)
    (format
     "\n\nThese aux methods are known:\n\n%s"
     (str/join
      "\n\n"
      (for [[qualifier dispatch-value->methods] (sort-by first qualifier->dispatch-value->methods)]
        (format
         "`%s` methods:\n\n%s"
         (pr-str qualifier)
         (str/join
          "\n\n"
          (for [[dispatch-value fns] dispatch-value->methods
                f                    fns]
            (describe-method dispatch-value f)))))))))
