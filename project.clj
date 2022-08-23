(defproject methodical "0.13.1"
  :url "https://github.com/camsaul/methodical"
  :min-lein-version "2.5.0"

  :license {:name "Eclipse Public License"
            :url  "https://raw.githubusercontent.com/camsaul/methodical/master/LICENSE"}

  :aliases
  {"repl"                      ["with-profile" "+repl" "repl"]
   ;; run lein deps with all dependencies from all the various profiles merged in. Useful for CI so we can cache
   ;; everything
   "deploy"                    ["with-profile" "+deploy" "deploy"]
   "all-deps"                  ["with-profile" "-user,+all-profiles" "deps"]
   "test"                      ["with-profile" "+test" "test"]
   "cloverage"                 ["with-profile" "+cloverage" "cloverage"]
   "profile"                   ["with-profile" "+profile" "run"]
   "eastwood"                  ["with-profile" "+eastwood" "eastwood"]
   "kibit"                     ["with-profile" "+kibit" "kibit"]
   "check-namespace-decls"     ["with-profile" "+check-namespace-decls" "check-namespace-decls"]
   "docstring-checker"         ["with-profile" "+docstring-checker" "docstring-checker"]
   "check-reflection-warnings" ["with-profile" "+reflection-warnings" "check"]
   "whitespace-linter"         ["with-profile" "+whitespace-linter"
                                "run" "-m" "clojure.main" "-e" (do
                                                                 (require 'whitespace-linter)
                                                                 (whitespace-linter/lint {:paths ["src" "test"]
                                                                                          :include-patterns [#".clj[cs]?$"]}))]
   ;; `lein lint` will run all linters. Except for reflecion warnings, use the script for that
   "lint"                      ["do" ["eastwood"] ["kibit"] ["check-namespace-decls"] ["cloverage"]
                                ["whitespace-linter"]["docstring-checker"]]}

  :dependencies
  [[mvxcvi/puget "1.3.2"]
   [pretty "1.0.5"]
   [potemkin "0.4.5"]]

  :profiles
  {:dev
   {:dependencies
    [[org.clojure/clojure "1.11.1"]
     [org.clojure/math.combinatorics "0.1.6"]
     [criterium "0.4.6"]
     [pjstadig/humane-test-output "0.11.0"]]

    :injections
    [(require 'pjstadig.humane-test-output)
     (pjstadig.humane-test-output/activate!)]

    :jvm-opts ["-Xverify:none"]

    :source-paths ["dev"]}

   :whitespace-linter
   {:dependencies [[com.camsaul/whitespace-linter "2022.07.21.02.09" :exclusions [org.clojure/clojure]]]}

   :repl
   {:global-vars {*warn-on-reflection* true}}

   :test
   {}

   :cloverage
   {:dependencies
    ;; Cloverage dependency is normally injected when the plugin is ran. By explicitly specifying it here we can
    ;; cache it in CI
    [[cloverage "1.2.4"]
     ;; Required by both Potemkin and Cloverage, but Potemkin uses an older version that breaks Cloverage's ablity to
     ;; understand certain forms. Explicitly specify newer version here.
     [riddley "0.2.0"]]

    :plugins
    [[lein-cloverage "1.2.2"]]

    ;; don't count ./dev stuff for code coverage calcualations.
    :source-paths ^:replace ["src"]

    :cloverage
    {:fail-threshold 90}}

   :profile
   {:main ^:skip-aot methodical.profile}

   :eastwood
   {:plugins
    [[jonase/eastwood "0.3.11" :exclusions [org.clojure/clojure]]]

    :eastwood
    {:config-files
     ["./.eastwood-config.clj"]

     :exclude-namespaces [:test-paths]

     ;; disabled for now until I figure out how to disable it in the one place it's popping up
     #_:remove-linters
     #_ [:unused-ret-vals]

     :add-linters
     [:unused-private-vars
      :unused-locals]}}

   :bikeshed
   {:dependencies
    ;; use latest tools.namespace instead of older version so we only need to fetch it once for all plugins.
    [[org.clojure/tools.namespace "1.3.0"]]

    :plugins
    [[lein-bikeshed "0.5.2"
      :exclusions [org.clojure/tools.namespace]]]}

   :kibit
   {:plugins
    [[lein-kibit "0.1.8"
      :exclusions [org.clojure/clojure]]]}

   :check-namespace-decls
   {:plugins               [[lein-check-namespace-decls "1.0.4"
                             :exclusions [org.clojure/clojure]]]
    :source-paths          ["test"]
    :check-namespace-decls {:prefix-rewriting false
                            :prune-ns-form    false}}

   :docstring-checker
   {:plugins
    [[docstring-checker "1.1.0"]]

    :docstring-checker
    {:include [#"^methodical"]
     :exclude [#"test" #"^methodical\.profile$"]}}

   ;; run `lein check-reflection-warnings` to check for reflection warnings
   :reflection-warnings
   {:global-vars {*warn-on-reflection* true}}

   :all-profiles
   [:test :cloverage :profile :eastwood :whitespace-linter :kibit :check-namespace-decls :docstring-checker
    :reflection-warnings
    {}]

   :deploy
   {:dependencies [[org.clojure/clojure "1.11.1"]]}}

  :deploy-repositories
  [["clojars"
    {:url           "https://clojars.org/repo"
     :username      :env/clojars_username
     :password      :env/clojars_password
     :sign-releases false}]])
