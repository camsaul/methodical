(ns methodical.impl.method-table.standard
  (:require methodical.interface
            [potemkin.types :as p.types]
            [pretty.core :refer [PrettyPrintable]])
  (:import methodical.interface.MethodTable))

(p.types/deftype+ StandardMethodTable [primary aux]
  PrettyPrintable
  (pretty [_]
    (cons
     'standard-method-table
     (apply
      concat
      (when (seq primary)
        [(count primary) 'primary])
      (for [[qualifier dispatch-val->methods] (sort-by first aux)
            :let                              [countt (reduce + (map count (vals dispatch-val->methods)))]
            :when                             (pos? countt)]
        [countt qualifier]))))

  Object
  (equals [_ another]
    (and (instance? StandardMethodTable another)
         (= primary (.primary ^StandardMethodTable another))
         (= aux (.aux ^StandardMethodTable another))))

  MethodTable
  (primary-methods [_]
    primary)

  (aux-methods [_]
    aux)

  (add-primary-method [this dispatch-val method]
    (let [new-primary (assoc primary dispatch-val method)]
      (if (= primary new-primary)
        this
        (StandardMethodTable. new-primary aux))))

  (remove-primary-method [this dispatch-val]
    (let [new-primary (dissoc primary dispatch-val)]
      (if (= primary new-primary)
        this
        (StandardMethodTable. new-primary aux))))

  (add-aux-method [this qualifier dispatch-value method]
    (let [new-aux (update-in aux [qualifier dispatch-value] (fn [existing-methods]
                                                              (if (contains? (set existing-methods) method)
                                                                existing-methods
                                                                (conj (vec existing-methods) method))))]
      (if (= aux new-aux)
        this
        (StandardMethodTable. primary new-aux))))

  (remove-aux-method [this qualifier dispatch-value method]
    (let [xforms  [(fn [aux]
                     (update-in aux [qualifier dispatch-value] (fn [defined-methods]
                                                                 (remove #(= % method) defined-methods))))
                   (fn [aux]
                     (cond-> aux
                       (empty? (get-in aux [qualifier dispatch-value]))
                       (update qualifier dissoc dispatch-value)))
                   (fn [aux]
                     (cond-> aux
                       (empty? (get aux qualifier))
                       (dissoc qualifier)))]
          new-aux (reduce (fn [aux xform] (xform aux)) aux xforms)]
      (if (= aux new-aux)
        this
        (StandardMethodTable. primary new-aux)))))
