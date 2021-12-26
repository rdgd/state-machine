(ns state-machine.core)

(defn get-state-by-name
  "Search state definitions for state by name"
  [state-defs state-name]
  (some
    (fn [{:keys [handler] :as sd}]
      (cond
        (= state-name (:name sd)) sd
        (sequential? handler) (get-state-by-name handler state-name)))
    state-defs))

(defn invoke-state
  ""
  [{:keys [schema acc state-name handler-arg] :as sm}]
  (if-let [{:keys [name handler stop-after on-fail stop-before automatic on-success]} (get-state-by-name schema state-name)]
    (cond
      (and stop-before (stop-before acc)) (assoc sm :state-name name)
      (sequential? handler) (invoke-state (assoc sm :state-name (-> handler first :name)))
      :else (let [res (handler sm)
                  success-args (assoc sm :state-name on-success :acc res)]
              (cond
                (and (not res) on-fail) (invoke-state (assoc sm :state-name on-fail))
                (and (not res) (not on-fail)) (throw (Error. (str "Missing on-fail handler for state " state-name)))
                (and stop-after (stop-after res)) success-args
                automatic (invoke-state success-args)
                :else success-args)))
    (throw (AssertionError. (str "State \"" state-name "\" does not exist")))))

(defn start
  "Convenience function to invoke first state in schema"
  [schema acc]
  (invoke-state {:schema schema
                 :acc acc
                 :state-name (-> schema first :name)}))
