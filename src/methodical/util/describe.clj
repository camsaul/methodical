(ns methodical.util.describe
  (:require [clojure.datafy :as datafy]
            [potemkin.types :as p.types]))

(p.types/defprotocol+ Describable
  (describe ^String [this]
    "Return a Markdown-formatted string description of a Methodical object, such as a multifn."))

(extend-protocol Describable
  nil
  (describe [_this]
    "nil")

  Object
  (describe [this]
    (pr-str (datafy/datafy this))))
