(ns state-machine.game
  (:require [state-machine.core :as c]
            [bond.james :as bond :refer [with-spy]]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]))

(defn stop?
  [x]
  (if (> x 9)
    (do
      (println "somebody won!")
      true)
    false))

(defn increase-value
  [wrapper state acc & [arg]]
  (println "state: " (:name state) " INCREASING: " acc)
  (or arg (inc acc)))

(defn setup
  [wrapper state acc & [arg]]
  (println "setup handler")
  (increase-value wrapper state acc arg))

(def first-nested (constantly true))
(def second-nested (constantly true))
(def last-nested (constantly true))

(def schema [{:name       :setup
              :only-once? true
              :handler    #'setup
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
              :handler    (fn [x y z xx] (println "This is the end") x)
              :stop-after (fn [z] z)
              :automatic  false}])

(deftest initialization-test
  (testing "Init function returns schema and history"
    (let [initted (c/init schema)]
      (is (= #{:history :states} (into #{} (keys initted))))
      (is (= [] (:history initted)))
      (is (= schema (:states initted))))))

(deftest start-test
  (testing "start function"
    (with-spy [setup]
      (let [sm (c/init schema)]
        (c/start sm 0)
        (is (= 1 (-> setup bond/calls count)) "automatically calls the first state handler")))))

(deftest manual-transition-test
  (testing "Automatic transition does not occur"
    (with-spy [first-nested]
      (let [sm (c/init schema)]
        (c/start sm 0)
        (is (= 0 (-> first-nested bond/calls count))))))
  (testing "Manual transition works"
    (with-spy [first-nested]
      (let [sm (c/init schema)
            {:keys [state-wrapper next acc]} (c/start sm 0)
            s (c/invoke-state state-wrapper acc next)]
        (is (= 1 (-> first-nested bond/calls count)))
        (is (= (-> s :state-wrapper :current-state-name) :start-nested))))))

(deftest nested-state-machine-test
  (testing "Nested state machines are reflected at the top-level wrapper"
    (let [sm (c/init schema)
          {:keys [state-wrapper next acc]} (c/start sm 0)
          s (c/invoke-state state-wrapper acc next)
          ss (c/invoke-state (:state-wrapper s) (:acc s) (:next s))]
      (-> ss keys)
        #_(is (= (-> ss :state-wrapper :current-state-name) :first-nested)))))

(comment
  (def initted (c/init schema))
  (def started (c/start initted 0 :setup))
  (def second-step (c/invoke-state (:state-wrapper started) (:acc started) (:next started) 921))
 (pprint started)
 (pprint second-step)
 ()
  )
