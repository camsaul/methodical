(ns methodical.macros.validate-arities-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :as t]
   [methodical.macros :as macros]
   [methodical.macros.validate-arities :as validate-arities]))

(t/deftest arglist-arities-test
  (t/are [arglists expected] (= expected
                                (#'validate-arities/arglist-arities
                                 (s/conform
                                  (s/+ :clojure.core.specs.alpha/param-list)
                                  (quote arglists))))

    ([x] [x y z & more])                         #{1 [:>= 3]}
    ([x] [x y z & {:as options}])                #{1 [:>= 3]}
    ([x] [x y z] [x y z & more])                 #{1 3 [:>= 3]}
    ([x] [x y z] [x y z k v] [x y z k v & more]) #{1 3 5 [:>= 5]}
    ([x] [x y z] [x y z k] [x y z k v & more])   #{1 3 4 [:>= 5]}
    ([x])                                        #{1}
    ([x y z & {:as options}])                    #{[:>= 3]}
    ([x y z & {:as options}])                    #{[:>= 3]}
    ([x] [x y z])                                #{1 3}
    ([x] [x y] [x y z & more])                   #{1 2 [:>= 3]}
    ([] [x] [x y z & more])                      #{0 1 [:>= 3]}
    ([x & [y z & {:as options}]])                #{[:>= 1]}
    ([x] [x y & more])                           #{1 [:>= 2]}
    ([a] [a b c] [a b c d])                      #{1 3 4}))

(t/deftest fn-tail-arities-test
  (t/are [arglists expected] (= expected
                                (#'validate-arities/fn-tail-arities (s/conform ::macros/fn-tail (quote arglists))))

    [([x] :ok)
     ([x y z & more] :ok)]
    #{1 [:>= 3]}

    [([x] :ok)
     ([x y z & {:as options}] :ok)]
    #{1 [:>= 3]}

    [([x] :ok)
     ([x y z] :ok)
     ([x y z & more] :ok)]
    #{1 3 [:>= 3]}

    [([x] :ok)
     ([x y z] :ok)
     ([x y z k v] :ok)
     ([x y z k v & more] :ok)]
    #{1 3 5 [:>= 5]}

    [([x] :ok)
     ([x y z] :ok)
     ([x y z k] :ok)
     ([x y z k v & more] :ok)]
    #{1 3 4 [:>= 5]}

    [([x] :ok)]
    #{1}

    [[x] :ok]
    #{1}

    [([x y z & {:as options}] :ok)]
    #{[:>= 3]}

    [[x y z & {:as options}] :ok]
    #{[:>= 3]}

    [([x] :ok) ([x y z] :ok)]
    #{1 3}

    [([x] :ok)
     ([x y] :ok)
     ([x y z & more] :ok)]
    #{1 2 [:>= 3]}

    [([] :ok)
     ([x] :ok)
     ([x y z & more] :ok)]
    #{0 1 [:>= 3]}

    [[x & [y z & {:as options}]]
     :ok]
    #{[:>= 1]}

    [([x] :ok)
     ([x y & more] :ok)]
    #{1 [:>= 2]}

    [([a] :ok)
     ([a b c] :ok)
     ([a b c d] :ok)]
    #{1 3 4}))

(t/deftest expand-arities-test
  (t/are [arities expected] (= expected
                               (#'validate-arities/expand-arities arities))
    #{}            #{}
    #{1}           #{1}
    #{1 2}         #{1 2}
    #{1 [:>= 3]}   #{1 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 :more}
    #{1 3 [:>= 3]} #{1 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 :more}))

(t/deftest diff-arities-test
  (t/are [actual-arities expected] (= expected
                                      (#'validate-arities/diff-arities #{1 [:>= 3]} actual-arities))
    ;; ok
    #{1 [:>= 3]}     nil
    #{1 3 [:>= 3]}   nil
    #{1 3 4 [:>= 5]} nil
    #{1 3 4 [:>= 4]} nil
    #{1 3 [:>= 4]}   nil
    ;; missing some required arities
    #{1}             {:required #{[:>= 3]}}
    #{[:>= 3]}       {:required #{1}}
    #{1 3}           {:required #{[:>= 3]}}
    #{1 3 4}         {:required #{[:>= 3]}}
    #{1 3 5 [:>= 5]} {:required #{[:>= 3]}} ; there's nothing that can handle 4 here.
    ;; has disallowed arities
    #{1 2 [:>= 3]}   {:disallowed #{2}}
    #{0 1 [:>= 3]}   {:disallowed #{0}}
    #{1 [:>= 2]}     {:disallowed #{[:>= 2]}} ; disallowed because this handles 2 and that's not allowed.
    #{[:>= 1]}       {:disallowed #{[:>= 1]}} ; same, this one handles 2
    ;; disallowed AND missing
    #{0 1}           {:required #{[:>= 3]}, :disallowed #{0}}))
