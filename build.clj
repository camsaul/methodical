(ns build
  (:require [clojure.java.shell :as sh]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [org.corfield.build :as bb]))

(def scm-url "git@github.com:camsaul/methodical.git")
(def lib     'io.github.camsaul/methodical)
(def version (str/trim (slurp "VERSION.txt")))

(defn sha [& _]
  (or (not-empty (System/getenv "GITHUB_SHA"))
      (not-empty (-> (sh/sh "git" "rev-parse" "HEAD")
                     :out
                     str/trim))))

(def default-options
  {:lib     lib
   :version version
   :scm     {:tag                 (sha)
             :connection          (str "scm:git:" scm-url)
             :developerConnection (str "scm:git:" scm-url)
             :url                 scm-url}})

(println "Options:")
(pprint/pprint default-options)
(println)

(defn build [opts]
  (doto (merge default-options opts)
    bb/clean
    bb/jar))

(defn install [opts]
  (printf "Installing %s to local Maven repository...\n" version)
  (bb/install (merge default-options opts)))

(defn build-and-install [opts]
  (build opts)
  (install opts))

(defn deploy [opts]
  (bb/deploy (merge default-options opts)))
