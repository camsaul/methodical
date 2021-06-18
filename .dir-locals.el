((nil . ((indent-tabs-mode . nil)       ; always use spaces for tabs
         (require-final-newline . t)))  ; add final newline on save
 (clojure-mode . (;; prefer keeping source width about ~118, GitHub seems to cut off stuff at either 119 or 120 and
                  ;; it's nicer to look at code in GH when you don't have to scroll back and forth
                  (fill-column . 118)
                  (clojure-docstring-fill-column . 118)
                  (eval . (define-clojure-indent
                            (p.types/defprotocol+ '(1 (:defn)))
                            (p.types/definterface+ '(1 (:defn)))
                            (p.types/def-abstract-type '(1 (:defn)))
                            (p.types/deftype+ '(2 nil nil (:defn)))
                            (p.types/defrecord+ '(2 nil nil (:defn))))))))
