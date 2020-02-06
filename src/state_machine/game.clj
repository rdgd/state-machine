(ns state-machine.game
  (:require [state-machine.core :as c]))

(defn somebody-won?
  [x]
  (if (> x 9)
    (do
      (println "somebody won!")
      true)
    false))

(defn increase-value
  [wrapper state x]
  (println "state: " (:name state) " INCREASING: " x)
  (inc x))

#_(defn setup
  [wrapper state acc]
  {:players [{:name "Ryan"
              :victory-points 0}
             {:name "Liz"
              :victory-points 0}]
   :board {}})

(def state (c/init [{:name       :setup
                     :handler increase-value                               ;setup
                     :stop-after somebody-won?
                     :automatic  true
                     :on-success :game-rounds}
                    {:name :game-rounds
                     :on-success :game-over
                     :automatic true
                     :handler [{:name :upkeep
                                :handler increase-value
                                :automatic true
                                :on-success :draw-card}
                               {:name :draw-card
                                :handler increase-value
                                :automatic true
                                :on-success :player-actions}
                               {:name :player-actions
                                :handler (fn [wrapper state x] {:next :pass-the-turn :value (inc x)})
                                :automatic true
                                :on-success :pass-the-turn}
                               {:name :pass-the-turn
                                :handler increase-value
                                :automatic true
                                :stop-after somebody-won?
                                :on-success :upkeep}]}
                    {:name       :game-over
                     :handler    (fn [x y z] (println "The game is over.") x)
                     :stop-after (fn [z] z)
                     :automatic  false}]))

(c/start state :setup 0)
