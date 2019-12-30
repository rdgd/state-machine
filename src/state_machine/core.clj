(ns state-machine.core)

(defn get-state-by-name
  [state-defs state-name & [context]]
  (some (fn [sd] (when (= state-name (:name sd)) sd)) state-defs))

(defn mk-history-entry
  [state acc status]
  (-> state
      (assoc :value acc :status status)
      (dissoc :handler :on-success :on-fail :unless :stop-after)))

(defn update-wrapper
  [{:keys [history] :as state-wrapper} acc current-state status & [dont-add-history?]]
  (merge
    state-wrapper
    {:current-state-name (:name current-state)
     :history            (if dont-add-history?
                           history
                           (conj history (mk-history-entry current-state acc status)))
     :current-value      acc}))

(defn init
  [state-defs]
  {:states state-defs
   :history []})

(defn merge-state-wrappers
  [wrapper-target wrapper]
  (assoc wrapper-target
    :history (concat (:history wrapper-target) (:history wrapper))
    :current-value (get wrapper :current-value)))

(defn invoke-state
  [state-wrapper acc & [state-name context]]
  (let [states (:states state-wrapper)]
    (if-let [{:keys [name handler stop-after on-fail unless automatic on-success] :as state} (get-state-by-name states state-name context)]
      (if (and unless (unless acc))
        (update-wrapper state-wrapper acc name true)
        (let [nested? (sequential? handler)
              res (if nested?
                    (invoke-state (init handler) acc (:name (first handler)) name)
                    (handler state-wrapper state acc))
              updated-state-wrapper (if nested?
                                      (update-wrapper (merge-state-wrappers state-wrapper res) (:current-value res) state (if res :success :fail))
                                      (update-wrapper state-wrapper res state (if res :success :fail)))]
          (cond
            (and (not res) on-fail) (invoke-state updated-state-wrapper acc on-fail)
            (and (not res) (not on-fail)) (throw (Error. (str "Failure occurred but no handler for state " state ". Data at time of failure " acc)))
            (and stop-after (stop-after res)) updated-state-wrapper
            automatic (invoke-state updated-state-wrapper res on-success)
            :else (partial invoke-state updated-state-wrapper res on-success))))
      (throw (AssertionError. (str "State \"" state-name "\" does not exist"))))))



(defn start
  [state name acc]
  (invoke-state state acc name))
