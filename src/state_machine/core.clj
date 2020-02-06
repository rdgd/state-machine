(ns state-machine.core)

(defn get-state-by-name
  "Search state definitions for state by name"
  [state-defs state-name]
  (some (fn [sd] (when (= state-name (:name sd)) sd)) state-defs))

(defn mk-history-entry
  "Create some data for logging in history sequence. Add the current value/state and status, and strip out some state
  data that doesn't serialize well (functions)"
  [state acc status]
  (-> state
      (assoc :value acc :status status)
      (dissoc :handler :on-success :on-fail :unless :stop-after)))

(defn update-wrapper
  "Update state wrapper with current value/state, add history entry, and current state name"
  [{:keys [history] :as state-wrapper} acc current-state status & [dont-add-history?]]
  (merge
    state-wrapper
    {:current-state-name (:name current-state)
     :history            (if dont-add-history?
                           history
                           (conj history (mk-history-entry current-state acc status)))
     :current-value      acc}))

(defn merge-state-wrappers
  "Function for combining nested state machine state with greater state."
  [wrapper-target wrapper]
  (println "NESTED CURRENT STATE:" (:current-state-name wrapper))
  (assoc wrapper-target
    :history (concat (:history wrapper-target) (:history wrapper))
    :current-value (get wrapper :current-value)))

(defn init
  "Initialize state wrapper with state definitions and empty history."
  [state-defs]
  {:states state-defs
   :history []})

(defn invoke-state
  "The heart of this application. Accepts a state wrapper, current value/state, state name and optionally a context "
  [state-wrapper acc state-name]
  (let [states (:states state-wrapper)]
    (if-let [{:keys [name handler stop-after on-fail unless automatic on-success] :as state} (get-state-by-name states state-name)]
      (if (and unless (unless acc))
        (update-wrapper state-wrapper acc name true)
        (let [nested? (sequential? handler)
              res (if nested?
                    (invoke-state (init handler) acc (:name (first handler)))
                    (handler state-wrapper state acc)) ;handlers can either return a value or a map with keys :value and :next
              next-step (or (:next res) on-success)
              res-value (or (:value res) res)
              updated-state-wrapper (if nested?
                                      (update-wrapper (merge-state-wrappers state-wrapper res-value) (:current-value res-value) state (if res-value :success :fail))
                                      (update-wrapper state-wrapper res-value state (if res-value :success :fail)))]
          (println "CURRENT STATE IS: " name)
          (cond
            (and (not res-value) on-fail) (invoke-state updated-state-wrapper acc on-fail)
            (and (not res-value) (not on-fail)) (throw (Error. (str "Failure occurred but no handler for state " state ". Data at time of failure " acc)))
            (and stop-after (stop-after res-value)) updated-state-wrapper
            automatic (invoke-state updated-state-wrapper res-value on-success)
            :else (partial invoke-state updated-state-wrapper res-value on-success))))
      (throw (AssertionError. (str "State \"" state-name "\" does not exist"))))))

(defn start
  [state name acc]
  (invoke-state state acc name))
