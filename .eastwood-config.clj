(disable-warning
 {:linter                      :unused-ret-vals
  :if-inside-macroexpansion-of #{'clojure.core/doseq}
  :within-depth                50
  :reason                      "doseq is done for side-effects. Of course the return values will be unused." })
