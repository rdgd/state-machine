(ns state-machine.core-test
  (:require [state-machine.core :as c]
            [bond.james :as bond :refer [with-spy]]
            [clojure.test :refer :all]))

;; Idea: Interceptors for before and after each output fn? Can be sequential to apply multiple middlewares
;; high-level/general middleware pattern too?
(defn make-automatic
  [schema]
  (mapv (fn [x]
          (if (sequential? (:output x))
            (assoc x :output (make-automatic (:output x)))
            (assoc x :automatic? true))) schema))

(defn halt?  [sm] (> (:acc sm) 100))
(defn inc-acc [sm] (update sm :acc inc))
(def start inc-acc)
(def first-nested inc-acc)
(def last-nested inc-acc)
(defn second-nested
  "The magic 4 is so that the automatic test run will reach a conclusion more quickly by satisfying the halt? condition. We have to have something there regardless, or we will npe."
  [{:keys [input acc] :as sm}]
  (assoc sm :acc (* (or input 4) acc)))

(def schema [{:name          :start
              :output        #'start
              :transition    :start-nested}
             {:name       :start-nested
              :transition :end
              :output [{:name :first-nested
                         :output #'first-nested
                         :transition :second-nested}
                        {:name :second-nested
                         :output #'second-nested
                         :transition :last-nested}
                        {:name :last-nested
                         :output #'last-nested
                         :halt-after? #'halt?
                         :transition :first-nested}]}
             {:name       :end
              :output    #(do (println "This is the end") %)
              :halt-after? (constantly true)
              :automatic  false}])

(deftest start-test
  (testing "start function"
    (with-spy [start]
      (c/start schema 0)
      (is (= 1 (-> start bond/calls count)) "automatically calls the first state output fn"))))

(deftest manual-transition-test
  (testing "Automatic transition does not occur"
    (with-spy [first-nested]
      (c/start schema 0)
      (is (= 0 (-> first-nested bond/calls count)))))
  (testing "Manual transition works"
    (with-spy [first-nested]
      (let [s (c/activate (c/start schema 0))]
        (is (= 1 (-> first-nested bond/calls count)))
        (is (= (:state-name s) :second-nested))))))

(deftest automatic-transition-test
  (testing "Automatic transition occurs"
    (let [res (c/start (make-automatic schema) 0)]
      (is (= :end (:state-name res))))))

(deftest nested-state-machine-test
  (testing "Nested state machines are reflected at the top-level wrapper"
    (let [started (c/start schema 0)
          s (c/activate started)
          ss (c/activate (assoc s :input 2))]
      (is (= (:state-name ss) :last-nested)))))

(deftest passing-inputs-test
  (testing "Passing inputs"
    (is (= (-> (c/start schema 0)
               (c/activate)
               (assoc :input 2)
               (c/activate)
               :acc) 4))))

(deftest get-state-by-name-test
  (testing "Getting state by name works"
    (is (= {:name :foo} (c/get-state-by-name [{:name :bar} {:name :foo}] :foo))))
  (testing "Getting nested states works"
    (is (= {:name :foo}
           (c/get-state-by-name [{:name :bar}
                                 {:name :baz :output [{:name :foo}]}] :foo)))))

(deftest get-state-parent-test
  (testing "Finds top level states with no parent"
    (is (nil? (c/get-state-parent schema :start-nested))))
  (testing "Finds nested state with parent"
    (is (= :start-nested (c/get-state-parent schema :last-nested)))))
