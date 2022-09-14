(ns methodical.util.dispatch
  "Common dispatch function definitions.")

(defn dispatch-on-first-arg
  "Create a dispatch function this will dispatch on the value of

  ```clj
  (dispatch-fn <first-arg>)
  ```

  and ignore all other args."
  [dispatch-fn]
  (fn dispatch-on-first-arg*
    ([a]
     (dispatch-fn a))
    ([a _b]
     (dispatch-fn a))
    ([a _b _c]
     (dispatch-fn a))
    ([a _b _c _d]
     (dispatch-fn a))
    ([a _b _c _d _e]
     (dispatch-fn a))
    ([a _b _c _d _e & _more]
     (dispatch-fn a))))

(defn dispatch-on-first-two-args
  "Create a dispatch function this will dispatch on the value of

  ```clj
  [(dispatch-fn <first-arg>) (dispatch-fn <second-arg>)]
  ```

  and ignore all other args."
  ([dispatch-fn]
   (dispatch-on-first-two-args dispatch-fn dispatch-fn))

  ([dispatch-fn-a dispatch-fn-b]
   (fn dispatch-on-first-two-args*
     ([a b]
      [(dispatch-fn-a a) (dispatch-fn-b b)])
     ([a b _c]
      (dispatch-on-first-two-args* a b))
     ([a b _c _d]
      (dispatch-on-first-two-args* a b))
     ([a b _c _d _e]
      (dispatch-on-first-two-args* a b))
     ([a b _c _d _e & _more]
      (dispatch-on-first-two-args* a b)))))

(defn dispatch-on-first-three-args
  "Create a dispatch function this will dispatch on the value of

  ```clj
  [(dispatch-fn <first-arg>) (dispatch-fn <second-arg>) (dispatch-fn <third-arg>)]
  ```

  and ignore all other args."
  ([dispatch-fn]
   (dispatch-on-first-three-args dispatch-fn dispatch-fn dispatch-fn))

  ([dispatch-fn-a dispatch-fn-b dispatch-fn-c]
   (fn dispatch-on-first-three-args*
     ([a b c]
      [(dispatch-fn-a a) (dispatch-fn-b b) (dispatch-fn-c c)])
     ([a b c _d]
      (dispatch-on-first-three-args* a b c))
     ([a b c _d _e]
      (dispatch-on-first-three-args* a b c))
     ([a b c _d _e & _more]
      (dispatch-on-first-three-args* a b c)))))

(defn dispatch-on-first-four-args
  "Create a dispatch function this will dispatch on the value of

  ```clj
  [(dispatch-fn <first-arg>) (dispatch-fn <second-arg>) (dispatch-fn <third-arg>) (dispatch-fn <fourth-arg>)]
  ```

  and ignore all other args."
  ([dispatch-fn]
   (dispatch-on-first-four-args dispatch-fn dispatch-fn dispatch-fn dispatch-fn))

  ([dispatch-fn-a dispatch-fn-b dispatch-fn-c dispatch-fn-d]
   (fn dispatch-on-first-four-args*
     ([a b c d]
      [(dispatch-fn-a a) (dispatch-fn-b b) (dispatch-fn-c c) (dispatch-fn-d d)])
     ([a b c d _e]
      (dispatch-on-first-four-args* a b c d))
     ([a b c d _e & _more]
      (dispatch-on-first-four-args* a b c d)))))
