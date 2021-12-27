(ns state-machine.core)

(defn get-state-by-name
  "Search state definitions for state by name"
  [schema state-name]
  (some
    (fn [{:keys [handler] :as sd}]
      (cond
        (= state-name (:name sd)) sd
        (sequential? handler) (get-state-by-name handler state-name)))
    schema))

(defn get-state-parent
  [schema state-name & [parent]]
  (reduce
    (fn [acc {:keys [name handler]}]
      (cond
        (= name state-name) parent
        (sequential? handler) (get-state-parent handler state-name name)
        :else acc)) nil schema))

(defn invoke-state
  [{:keys [schema state-name] :as sm}]
  (if-let [{:keys [name handler validator stop-after on-fail stop-before automatic? on-success]}
           (get-state-by-name schema state-name)]
    (cond
      (and stop-before (stop-before sm)) (assoc sm :state-name name)
      (sequential? handler) (invoke-state (assoc sm :state-name (-> handler first :name)))
      :else (let [res (handler sm)
                  success-args (assoc sm :state-name on-success :acc (:acc res))]
              (cond
                (and (not res) on-fail) (invoke-state (assoc sm :state-name on-fail))
                (and (not res) (not on-fail)) (throw (Error. (str "Missing on-fail handler for state " state-name)))
                (and validator (not (validator res))) sm
                (and stop-after (stop-after res)) (if-let [sn (get-state-parent schema state-name)]
                                                    (assoc success-args :state-name (:on-success (get-state-by-name schema sn)))
                                                    success-args)
                automatic? (invoke-state success-args)
                :else success-args)))
    (throw (AssertionError. (str "State \"" state-name "\" does not exist")))))

(defn start
  "Convenience function to invoke first state in schema"
  [schema acc]
  (invoke-state {:schema schema
                 :acc acc
                 :state-name (-> schema first :name)}))
