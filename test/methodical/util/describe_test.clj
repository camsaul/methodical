(ns methodical.util.describe-test
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [methodical.core :as m]
   [methodical.util.describe :as describe]))

(defonce ^:private dispatch-first
  (fn [x _y]
    (keyword x)))

(m/defmulti mf
  "mf is a great multimethod."
  {:arglists '([x y])}
  dispatch-first)

(m/defmethod mf :default
  "Here is a docstring."
  [x y]
  {:x x, :y y})

(m/defmethod mf :before [:x :default]
  "Another docstring."
  [_x y]
  y)

(m/defmethod mf :around [:x :y]
  [x y]
  (next-method x y))

(m/prefer-method! #'mf :x :y)

(def ^:private expected-description
  ["mf is defined in [[methodical.util.describe-test]] (methodical/util/describe_test.clj:12)."
   ""
   "It caches methods using a `methodical.impl.cache.watching.WatchingCache`."
   ""
   "It uses the method combination `methodical.impl.combo.threaded.ThreadingMethodCombination`"
   "with the threading strategy `:thread-last`."
   ""
   "It uses the dispatcher `methodical.impl.dispatcher.multi_default.MultiDefaultDispatcher`"
   "with hierarchy `#'clojure.core/global-hierarchy`"
   "and prefs `{:x #{:y}}`."
   ""
   "The default value is `:default`."
   ""
   "It uses the method table `methodical.impl.method_table.standard.StandardMethodTable`."
   ""
   "These primary methods are known:"
   ""
   "* `:default`, defined in [[methodical.util.describe-test]] (methodical/util/describe_test.clj:17) "
   "  "
   "  It has the following documentation:"
   "  "
   "  Here is a docstring."
   ""
   "These aux methods are known:"
   ""
   "`:around` methods:"
   ""
   "* `[:x :y]`, defined in [[methodical.util.describe-test]] (methodical/util/describe_test.clj:27) "
   ""
   "`:before` methods:"
   ""
   "* `[:x :default]`, defined in [[methodical.util.describe-test]] (methodical/util/describe_test.clj:22) "
   "  "
   "  It has the following documentation:"
   "  "
   "  Another docstring."])

(t/deftest describe-test
  (t/is (= expected-description
           (str/split-lines (describe/describe mf)))))

(t/deftest update-docstrings-test
  (t/is (= (concat
            ["mf is a great multimethod."
             ""]
            expected-description)
           (str/split-lines (:doc (meta #'mf))))))
