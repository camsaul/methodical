(ns methodical.util.describe
  (:require [clojure.datafy :as datafy]
            [potemkin.types :as p.types]))

(p.types/defprotocol+ Describeable
  (describe ^String [this]
    "Return a string description of a Methodical object, such as a multifn."))

(extend-protocol Describeable
  nil
  (describe [_this]
    "nil")

  Object
  (describe [this]
    (pr-str (datafy/datafy this))))
