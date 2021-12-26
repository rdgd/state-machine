(ns state-machine.game
  (:require [state-machine.core :as c]
            [bond.james :as bond :refer [with-spy]]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]))

;; Idea: Interceptors for before and after each handler? Can be sequential to apply multiple middlewares
;; high-level/general middleware pattern too?

(defn stop?
  [x]
  (if (> x 9)
    (do
      (println "somebody won!")
      true)
    false))

(def start (constantly true))
(def first-nested (constantly true))
(def last-nested (constantly true))

(defn second-nested
  [{:keys [handler-arg]}]
  handler-arg)

(def schema [{:name       :start
              :only-once? true
              :handler    #'start
              :automatic  false
              :on-success :start-nested}
             {:name       :start-nested
              :on-success :end
              :automatic  false
              :handler [{:name :first-nested
                         :handler #'first-nested
                         :automatic false
                         :on-success :second-nested}
                        {:name :second-nested
                         :handler #'second-nested
                         :automatic false
                         :on-success :last-nested}
                        {:name :last-nested
                         :handler #'last-nested
                         :automatic false
                         :stop-after #'stop?
                         :on-success :first-nested}]}
             {:name       :end
              :handler    (fn [x] (println "This is the end") x)
              :stop-after (fn [x] x)
              :automatic  false}])

(deftest start-test
  (testing "start function"
    (with-spy [start]
      (:state-name (c/start schema 0))
      (is (= 1 (-> start bond/calls count)) "automatically calls the first state handler"))))

(deftest manual-transition-test
  (testing "Automatic transition does not occur"
    (with-spy [first-nested]
      (c/start schema 0)
      (is (= 0 (-> first-nested bond/calls count)))))
  (testing "Manual transition works"
    (with-spy [first-nested]
      (let [s (c/invoke-state (c/start schema 0))]
        (is (= 1 (-> first-nested bond/calls count)))
        (is (= (:state-name s) :second-nested))))))

(deftest nested-state-machine-test
  (testing "Nested state machines are reflected at the top-level wrapper"
    (let [started (c/start schema 0)
          s (c/invoke-state started)
          ss (c/invoke-state (assoc s :handler-arg "foobar"))]
      (is (= (:state-name ss) :last-nested)))))

(deftest passing-handler-args-test
  (testing "Passing handler args"
    (let [started (c/start schema 0)
          s (c/invoke-state started)
          ss (c/invoke-state (assoc s :handler-arg "foobar"))]
      (is (= (:acc ss) "foobar")))))

(deftest get-state-by-name-test
  (testing "Getting state by name works"
    (is (= {:name :foo} (c/get-state-by-name [{:name :bar} {:name :foo}] :foo))))
  (testing "Getting nested states works"
    (is (= {:name :foo}
           (c/get-state-by-name [{:name :bar}
                                 {:name :baz :handler [{:name :foo}]}] :foo)))))
