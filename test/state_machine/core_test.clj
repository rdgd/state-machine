(ns state-machine.core-test
  (:require [clojure.test :refer :all]
            [state-machine.core :as sm]))

(def state-data [{:name       :first-state
                  :handler    inc
                  :automatic  true
                  :on-success :second-state}
                 {:name       :second-state
                  :handler    inc
                  :automatic  true
                  :stop-after (fn [x] (>= x 2))
                  :on-success :first-state}])

(deftest init-state-machine
  (testing "Initialize a state machine"
    (let [inited (sm/init state-data)]
      (is (= (:states inited) state-data))
      (is (= (:history inited) [])))))

(deftest state-machine-history
  (testing "Invoking a state, adds it to history"
    (let [inited (sm/init state-data)]
      (is (= 2 (count (:history (sm/start inited :first-state 0)))))
      (is (= (:history inited) [])))))
