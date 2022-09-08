(ns methodical.util.trace-test
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [methodical.core :as m]
            [methodical.util.trace :as trace]
            [pretty.core :as pretty]))

(def ^:private h
  (-> (make-hierarchy)
      (derive :parrot :bird)
      (derive :parakeet :parrot)))

(def ^:private my-multimethod
  (-> (m/default-multifn (fn [x m] [(type x) (:type m)]) :hierarchy #'h)
      (m/add-primary-method :default
                            (fn [_ x m] (assoc m :x x)))
      (m/add-primary-method [:default :bird]
                            (fn [next-method x m]
                              (next-method x (assoc m :bird? true))))
      (m/add-aux-method :before [:default :parrot]
                        (fn [_ m] (assoc m :parrot? true)))
      (m/add-aux-method :after [Object :default]
                        (fn [_ m] (assoc m :object? true)))
      (m/add-aux-method :around [CharSequence :default]
                        (fn [next-method x m]
                          (next-method x (assoc m :char-seq? true))))))

(t/deftest return-same-value-test
  (t/testing "Should return the same result as calling the multimethod without tracing"
    (with-out-str
      (t/is (= {:x 1, :object? true}
               (my-multimethod 1 {})
               (trace/trace my-multimethod 1 {})))
      (t/is (= {:x nil}
               (my-multimethod nil {})
               (trace/trace my-multimethod nil {})))
      (t/is (= {:type :parrot, :bird? true, :parrot? true, :x 1, :object? true}
               (my-multimethod 1 {:type :parrot})
               (trace/trace my-multimethod 1 {:type :parrot})))
      (t/is (= {:type :parakeet, :bird? true, :parrot? true, :x 1, :object? true}
               (my-multimethod 1 {:type :parakeet})
               (trace/trace my-multimethod 1 {:type :parakeet})))
      (t/is (= {:type :parakeet, :bird? true, :parrot? true, :x "str", :object? true, :char-seq? true}
               (my-multimethod "str" {:type :parakeet})
               (trace/trace my-multimethod "str" {:type :parakeet}))))))

(defmacro ^:private trace-output [& args]
  `(binding [trace/*color* false]
     (str/split-lines (str/trim (with-out-str (trace/trace ~@args))))))

(t/deftest output-test
  (t/is (= ["0: (my-multimethod 1 {})"
            "  1: (#primary-method<:default> nil 1 {})"
            "  1> {:x 1}"
            "  1: (#aux-method<:after [java.lang.Object :default]> 1 {:x 1})"
            "  1> {:object? true, :x 1}"
            "0> {:object? true, :x 1}"]
           (trace-output my-multimethod 1 {})))
  (t/is (= ["0: (my-multimethod nil {})"
            "  1: (#primary-method<:default> nil nil {})"
            "  1> {:x nil}"
            "0> {:x nil}"]
           (trace-output my-multimethod nil {})))
  (t/is (= ["0: (my-multimethod 1 {:type :parrot})"
            "  1: (#aux-method<:before [:default :parrot]> 1 {:type :parrot})"
            "  1> {:parrot? true, :type :parrot}"
            "  1: (#primary-method<[:default :bird]>"
            "      #primary-method<:default>"
            "      1"
            "      {:parrot? true, :type :parrot})"
            "    2: (#primary-method<:default> nil 1 {:bird? true, :parrot? true, :type :parrot})"
            "    2> {:bird? true, :parrot? true, :type :parrot, :x 1}"
            "  1> {:bird? true, :parrot? true, :type :parrot, :x 1}"
            "  1: (#aux-method<:after [java.lang.Object :default]>"
            "      1"
            "      {:bird? true, :parrot? true, :type :parrot, :x 1})"
            "  1> {:bird? true, :object? true, :parrot? true, :type :parrot, :x 1}"
            "0> {:bird? true, :object? true, :parrot? true, :type :parrot, :x 1}"]
           (trace-output my-multimethod 1 {:type :parrot}))))

(def ^:private lots-of-args-multifn
  (-> (m/default-multifn
       (fn [a b c d e _f] [a (class b) c d e]))
      (m/add-primary-method :default
                            (fn [_ a _ _ _ _ f] {:a a, :f f}))
      (m/add-primary-method [::x :default :default :default :default]
                            (fn [_ a _ _ _ _ f] {:x a, :f f}))))

(t/deftest lots-of-args-test
  (t/testing "> 4 args"
    (t/is (= {:x ::x, :f :f}
             (lots-of-args-multifn ::x :b :c :d :e :f)))
    (t/is (= ["0: (lots-of-args-multifn :methodical.util.trace-test/x :b :c :d :e :f)"
              "  1: (#primary-method<[:methodical.util.trace-test/x :default :default :default :default]>"
              "      #primary-method<:default>"
              "      :methodical.util.trace-test/x"
              "      :b"
              "      :c"
              "      :d"
              "      :e"
              "      :f)"
              "  1> {:f :f, :x :methodical.util.trace-test/x}"
              "0> {:f :f, :x :methodical.util.trace-test/x}"]
             (trace-output lots-of-args-multifn ::x :b :c :d :e :f)))))

(m/defmulti my=
  {:arglists '([x y])}
  (fn [x y]
    [(class x) (class y)]))

(m/defmethod my= :default
  [x y]
  (= x y))

(m/defmethod my= [clojure.lang.AFunction Object]
  [pred x]
  (pred x))

(t/deftest function-arg-test
  (t/testing "Function arguments should not be printed as nil (#86)"
    ;; depending on the Clojure version [[int?`]] might get printed like
    ;;
    ;;    #function[clojure.core/int?]
    ;;
    ;; or it might get printed like
    ;;
    ;;    #object[clojure.core$int_QMARK_ 0x3e07ccbf \"clojure.core$int_QMARK_@3e07ccbf\"]
    ;;
    ;; so to make this test pass in either situation hijack `pr-str` which we use to print functions.
    (let [orig-pr-str pr-str]
      (with-redefs [pr-str (fn [x]
                             (if (identical? x int?)
                               "#function[clojure.core/int?]"
                               (orig-pr-str x)))]
        ;; it might be #function[clojure.core/int?] or might not. Depends on the Clojure version I guess
        (t/is (= ["0: (my= #function[clojure.core/int?] 100)"
                  "  1: (#primary-method<[clojure.lang.AFunction java.lang.Object]>"
                  "      #primary-method<:default>"
                  "      #function[clojure.core/int?]"
                  "      100)"
                  "  1> true"
                  "0> true"]
                 (trace-output my= int? 100)))))))

(defrecord PrettyRecord [x]
  pretty/PrettyPrintable
  (pretty [_this]
    (list '->PrettyRecord x)))

(m/defmulti pretty-record
  class)

(m/defmethod pretty-record :default
  [x]
  x)

(t/deftest pretty-print-PrettyPrintable-test
  (t/testing "Use [[pretty.core/pretty]] to print things when possible"
    (t/is (= "(->PrettyRecord {:a 1})"
             (pr-str (pretty-record (->PrettyRecord {:a 1})))))
    (t/is (= ["0: (pretty-record (->PrettyRecord {:a 1}))"
              "  1: (#primary-method<:default> nil (->PrettyRecord {:a 1}))"
              "  1> (->PrettyRecord {:a 1})"
              "0> (->PrettyRecord {:a 1})"]
             (trace-output pretty-record (->PrettyRecord {:a 1}))))))
