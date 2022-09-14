(ns methodical.util.dispatch-test
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [methodical.core :as m]))

(t/deftest dispatch-on-first-arg-test
  (let [df (m/dispatch-on-first-arg keyword)]
    (t/are [args] (= :a
                     (apply df args))
      ["a"]
      ["a" "b"]
      ["a" "b" "c"]
      ["a" "b" "c" "d"]
      ["a" "b" "c" "d" "e"]
      ["a" "b" "c" "d" "e" "f"])))

(t/deftest dispatch-on-first-two-args-test
  (let [df (m/dispatch-on-first-two-args keyword)]
    (t/are [args] (= [:a :b]
                     (apply df args))
      ["a" "b"]
      ["a" "b" "c"]
      ["a" "b" "c" "d"]
      ["a" "b" "c" "d" "e"]
      ["a" "b" "c" "d" "e" "f"]))
  (t/testing "different dispatch functions"
    (let [df (m/dispatch-on-first-two-args keyword str)]
      (t/are [args] (= [:a "b"]
                       (apply df args))
        ["a" "b"]
        ["a" "b" "c"]
        ["a" "b" "c" "d"]
        ["a" "b" "c" "d" "e"]
        ["a" "b" "c" "d" "e" "f"]))))

(t/deftest dispatch-on-first-three-args-test
  (let [df (m/dispatch-on-first-three-args keyword)]
    (t/are [args] (= [:a :b :c]
                     (apply df args))
      ["a" "b" "c"]
      ["a" "b" "c" "d"]
      ["a" "b" "c" "d" "e"]
      ["a" "b" "c" "d" "e" "f"]))
  (t/testing "different dispatch functions"
    (let [df (m/dispatch-on-first-three-args keyword str some?)]
      (t/are [args] (= [:a "b" true]
                       (apply df args))
        ["a" "b" "c"]
        ["a" "b" "c" "d"]
        ["a" "b" "c" "d" "e"]
        ["a" "b" "c" "d" "e" "f"]))))

(t/deftest dispatch-on-first-four-args-test
  (let [df (m/dispatch-on-first-four-args keyword)]
    (t/are [args] (= [:a :b :c :d]
                     (apply df args))
      ["a" "b" "c" "d"]
      ["a" "b" "c" "d" "e"]
      ["a" "b" "c" "d" "e" "f"]))
  (t/testing "Different dispatch functions"
    (let [df (m/dispatch-on-first-four-args keyword str some? str/upper-case)]
      (t/are [args] (= [:a "b" true "D"]
                       (apply df args))
        ["a" "b" "c" "d"]
        ["a" "b" "c" "d" "e"]
        ["a" "b" "c" "d" "e" "f"]))))
