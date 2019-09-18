(ns methodical.impl.dispatcher.everything-test
  (:require [clojure
             [string :as str]
             [test :as t]]
            [methodical.core :as m]))

(t/deftest everything-dispatcher-test
  (t/testing "everything dispatcher"
    (let [f (-> (m/multifn
                 (m/standard-multifn-impl
                  (m/do-method-combination)
                  (m/everything-dispatcher)
                  (m/standard-method-table)))
                (m/add-primary-method :scheduler
                                      (fn [calls]
                                        (swap! calls conj "Shutdown Scheduler")))
                (m/add-primary-method :server
                                      (fn [calls]
                                        (swap! calls conj "Shutdown Web Server")))
                (m/prefer-method :server :scheduler)
                (m/add-aux-method :around :initiate
                                  (fn [next-method calls]
                                    (swap! calls conj "Initiating shutdown...")
                                    (next-method calls))))]
      (t/is (= ["Initiating shutdown..."
                "Shutdown Web Server"
                "Shutdown Scheduler"]
               (let [calls (atom [])]
                 (f calls)
                 @calls))
            "We should be able to create a multifn using the everything dispatcher"))))

(m/defmulti ^:private shutdown!
  :none
  :dispatcher (m/everything-dispatcher)
  :combo (m/do-method-combination))

(m/defmethod shutdown! :task-scheduler
  []
  (println "Shutting down task scheduler..."))

(m/defmethod shutdown! :web-server
  []
  (println "Shutting down web server..."))

(m/prefer-method! #'shutdown! :web-server :task-scheduler)

(m/defmethod shutdown! :around :initiate
  []
  (println "Initiating shutdown...")
  (next-method))

(t/deftest e2e-test
  (t/is (= ["Initiating shutdown..."
            "Shutting down web server..."
            "Shutting down task scheduler..."]
           (str/split-lines (with-out-str (shutdown!))))))
