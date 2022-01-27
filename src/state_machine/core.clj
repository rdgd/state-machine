(ns state-machine.core)

(defn get-state-by-name
  [schema state-name]
  (some
    (fn [{:keys [output] :as sd}]
      (cond
        (= state-name (:name sd)) sd
        (sequential? output) (get-state-by-name output state-name)))
    schema))

(defn get-state-parent
  [schema state-name & [parent]]
  (reduce
    (fn [acc {:keys [name output]}]
      (cond
        (= name state-name) parent
        (sequential? output) (get-state-parent output state-name name)
        :else acc)) nil schema))

(defn next-state
  [sm transition output-val]
  (if (keyword? transition)
    transition
    (transition sm output-val)))

(defn activate
  [{:keys [schema state-name] :as sm}]
  (if-let [{:keys [name output valid? halt-after? halt-before? automatic? transition]}
           (get-state-by-name schema state-name)]
    (cond
      (and halt-before? (halt-before? sm)) (assoc sm :state-name name)
      (sequential? output) (activate (assoc sm :state-name (-> output first :name)))
      :else (let [output-val (output sm)
                  success-args (-> sm
                                   (assoc :state-name (next-state sm transition output-val) :acc (:acc output-val))
                                   #_(update :history (fn [history] (conj (or history []) (dissoc sm :schema)))))]
              (cond
                (and valid? (not (valid? output-val))) sm
                (and halt-after? (halt-after? output-val))
                (if-let [sn (get-state-parent schema state-name)]
                  (assoc success-args :state-name (next-state sm (:transition (get-state-by-name schema sn)) output-val))
                  success-args)
                automatic? (activate success-args)
                :else success-args)))
    (throw (AssertionError. (str "State \"" state-name "\" does not exist")))))

(defn start
  "Convenience function to invoke first state in schema"
  [schema acc]
  (activate {:schema schema
             :acc acc
             :state-name (-> schema first :name)}))
