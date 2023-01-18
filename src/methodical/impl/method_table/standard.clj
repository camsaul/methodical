(ns methodical.impl.method-table.standard
  (:require
   [clojure.core.protocols :as clojure.protocols]
   [methodical.impl.method-table.common :as method-table.common]
   [methodical.interface]
   [methodical.util.describe :as describe]
   [pretty.core :as pretty])
  (:import
   (methodical.interface MethodTable)))

(set! *warn-on-reflection* true)

(comment methodical.interface/keep-me)

(defn- dispatch-value-map
  "Create a representation of `primary` and `aux` methods using their dispatch values for pretty-printing a method table."
  [primary aux]
  (not-empty
   (merge
    (when-let [dvs (not-empty (vec (sort (keys primary))))]
      {:primary dvs})
    (when-let [aux-methods (not-empty
                            (into {} (for [[qualifier dv->fns] aux
                                           :let                [dvs (for [[dv fns] dv->fns
                                                                          _f       fns]
                                                                      dv)]
                                           :when               (seq dvs)]
                                       [qualifier (vec (sort dvs))])))]
      {:aux aux-methods}))))

(deftype StandardMethodTable [primary aux]
  pretty/PrettyPrintable
  (pretty [_]
    (if-let [m (not-empty (dispatch-value-map primary aux))]
      (list 'standard-method-table m)
      (list 'standard-method-table)))

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
    (let [new-primary (assoc primary dispatch-val (vary-meta method assoc :dispatch-value dispatch-val))]
      (if (= primary new-primary)
        this
        (StandardMethodTable. new-primary aux))))

  (remove-primary-method [this dispatch-val]
    (let [new-primary (dissoc primary dispatch-val)]
      (if (= primary new-primary)
        this
        (StandardMethodTable. new-primary aux))))

  (add-aux-method [this qualifier dispatch-value method]
    (let [new-aux (update-in aux
                             [qualifier dispatch-value]
                             (fn [existing-methods]
                               (if (contains? (set existing-methods) method)
                                 existing-methods
                                 (conj (vec existing-methods)
                                       (vary-meta method assoc :dispatch-value dispatch-value)))))]
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
        (StandardMethodTable. primary new-aux))))

  clojure.protocols/Datafiable
  (datafy [this]
    {:class   (class this)
     :primary (method-table.common/datafy-primary-methods primary)
     :aux     (method-table.common/datafy-aux-methods aux)})

  describe/Describable
  (describe [this]
    (str (format "It uses the method table `%s`." (.getCanonicalName (class this)))
         (method-table.common/describe-primary-methods primary)
         (method-table.common/describe-aux-methods aux))))
