(ns state-machine.core-test
  (:require [state-machine.core :as c]
            [bond.james :as bond :refer [with-spy]]
            [clojure.test :refer :all]))

;; Idea: Interceptors for before and after each handler? Can be sequential to apply multiple middlewares
;; high-level/general middleware pattern too?

(defn make-automatic
  [schema]
  (mapv (fn [x]
          (if (sequential? (:handler x))
            (assoc x :handler (make-automatic (:handler x)))
            (assoc x :automatic? true))) schema))

(defn stop?
  [sm]
  (> (:acc sm) 100))

(defn inc-acc [sm]
  (update sm :acc inc))

(def start inc-acc)
(def first-nested inc-acc)
(def last-nested inc-acc)

(defn second-nested
  [{:keys [handler-arg acc] :as sm}]
  (assoc sm :acc (* (or handler-arg 4) acc)))

(def schema [{:name          :start
              :handler       #'start
              :on-success    :start-nested}
             {:name       :start-nested
              :on-success :end
              :handler [{:name :first-nested
                         :handler #'first-nested
                         :on-success :second-nested}
                        {:name :second-nested
                         :handler #'second-nested
                         :on-success :last-nested}
                        {:name :last-nested
                         :handler #'last-nested
                         :stop-after #'stop?
                         :on-success :first-nested}]}
             {:name       :end
              :handler    (fn [x] (println "This is the end") x)
              :stop-after (fn [x] x)
              :automatic  false}])

(deftest get-state-parent-test
  (testing "Finds top level states with no parent"
    (is (nil? (c/get-state-parent schema :start-nested))))
  (testing "Finds nested state with parent"
    (is (= :start-nested (c/get-state-parent schema :last-nested)))))

(deftest start-test
  (testing "start function"
    (with-spy [start]
      (c/start schema 0)
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

(deftest automatic-transition-test
  (testing "Automatic transition occurs"
    (let [res (c/start (make-automatic schema) 0)]
      (is (= :end (:state-name res))))))

(deftest nested-state-machine-test
  (testing "Nested state machines are reflected at the top-level wrapper"
    (let [started (c/start schema 0)
          s (c/invoke-state started)
          ss (c/invoke-state (assoc s :handler-arg 2))]
      (is (= (:state-name ss) :last-nested)))))

(deftest passing-handler-args-test
  (testing "Passing handler args"
    (let [started (c/start schema 0)
          s (c/invoke-state started)
          ss (c/invoke-state (assoc s :handler-arg 2))]
      (is (= (:acc ss) 4)))))

(deftest get-state-by-name-test
  (testing "Getting state by name works"
    (is (= {:name :foo} (c/get-state-by-name [{:name :bar} {:name :foo}] :foo))))
  (testing "Getting nested states works"
    (is (= {:name :foo}
           (c/get-state-by-name [{:name :bar}
                                 {:name :baz :handler [{:name :foo}]}] :foo)))))
