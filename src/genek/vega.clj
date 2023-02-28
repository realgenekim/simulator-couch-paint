(ns genek.vega)

(defn points
  " given states, generate data points suitable to feed into vega-lite"
  [states]
  (->> states
    ;identity
    (map (fn [s]
           {:turn                (-> s :turn)
            :furnture-in-storage (or
                                   (-> s :furniture :in-storage)
                                   0)}))))


(defn vega-plot-furniture-vs-time
  [vs]
  {"$schema" "https://vega.github.io/schema/vega-lite/v5.json"
   :data {:values vs}
   :mark {:type :line}
   :encoding {:x {:field :turn
                  :type :quantitative}
              :y {:field :furnture-in-storage
                  :type :quantitative}}})

(defn states>furniture-plot
  [states]
  (merge {:width 400
          :height 100}
    (vega-plot-furniture-vs-time states)))