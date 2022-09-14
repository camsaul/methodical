(ns methodical.datafy-test
  (:require
   [clojure.datafy :as datafy]
   [clojure.test :as t]
   [methodical.core :as m]))

(defonce ^:private dispatch-first
  (fn [x _y]
    (keyword x)))

(m/defmulti mf
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

(t/deftest datafy-test
  (t/is (= {:ns           'methodical.datafy-test
            :name         'methodical.datafy-test/mf
            :file         "methodical/datafy_test.clj"
            :line         11
            :column       1
            :arglists     '([x y])
            :class        methodical.impl.standard.StandardMultiFn
            :combo        {:class          methodical.impl.combo.threaded.ThreadingMethodCombination
                           :threading-type :thread-last}
            :dispatcher   {:class         methodical.impl.dispatcher.multi_default.MultiDefaultDispatcher
                           :dispatch-fn   methodical.datafy-test/dispatch-first
                           :default-value :default
                           :hierarchy     #'clojure.core/global-hierarchy
                           :prefs         {:x #{:y}}}
            :method-table {:class   methodical.impl.method_table.standard.StandardMethodTable
                           :primary {:default
                                     {:ns       'methodical.datafy-test
                                      :name     'methodical.datafy-test/mf-primary-method-default
                                      :doc      "Here is a docstring."
                                      :file     "methodical/datafy_test.clj"
                                      :line     15
                                      :column   1
                                      :arglists '([next-method x y])
                                      :multifn  #'methodical.datafy-test/mf}}
                           :aux     {:before {[:x :default] [{:ns                    'methodical.datafy-test
                                                              :name                  'methodical.datafy-test/mf-before-method-x-default
                                                              :doc                   "Another docstring."
                                                              :file                  "methodical/datafy_test.clj"
                                                              :column                1
                                                              :line                  20
                                                              :arglists              '([_x y])
                                                              :multifn               #'methodical.datafy-test/mf
                                                              :methodical/unique-key 'methodical.datafy-test}]}
                                     :around {[:x :y] [{:ns                    'methodical.datafy-test
                                                        :name                  'methodical.datafy-test/mf-around-method-x-y
                                                        :file                  "methodical/datafy_test.clj"
                                                        :column                1
                                                        :line                  25
                                                        :arglists              '([next-method x y])
                                                        :multifn               #'methodical.datafy-test/mf
                                                        :methodical/unique-key 'methodical.datafy-test}]}}}
            :cache        {:class methodical.impl.cache.watching.WatchingCache
                           :cache {:class methodical.impl.cache.simple.SimpleCache
                                   :cache {}}
                           :refs  #{#'clojure.core/global-hierarchy}}}
           (datafy/datafy mf))))
