(ns genek.vega
  (:require
    [clojure.data.json :as json]
    [genek.my-darkstar :as darkstar]))

; {
;  "data": {"url": "data/stocks.csv"},
;            "values": [
;                       {"x": 0, "y": 28, "c":0}, {"x": 0, "y": 20, "c":1},
;  "transform": [{"filter": "datum.symbol==='GOOG'"}],
;  "mark": "line",
;  "encoding": {
;    "x": {"field": "date", "type": "temporal"},
;    "y": {"field": "price", "type": "quantitative"}
;  }
;  }
;}


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
  " input: data in [{:x :y} ...]
    output: vega "
  [vs]
  {"$schema" "https://vega.github.io/schema/vega-lite/v5.json"
   :data {:values vs}
   :mark {:type :line}
   :encoding {:x {:field :turn
                  :type :quantitative}
              :y {:field :furnture-in-storage
                  :type :quantitative}}})

(defn vega-plot-furniture-vs-time-highlight-turn
  " input: data in [{:x :y} ...]
    output: vega "
  [turn vs]
  (let [vega (vega-plot-furniture-vs-time vs)]
    (-> vega
      (assoc-in [:encoding :color]
        {:value "red"})
      (assoc-in [:mark :point]
        "true"))))



(defn states>furniture-plot
  " input: states
    output: vega furniture plot"
  [states]
  (merge {:width 400
          :height 100}
    (vega-plot-furniture-vs-time states)))

(defn vega>svg
  [v]
  (->> v
    (json/write-str)
    darkstar/vega-lite-spec->svg))

;
; leftover
;

(defn histogram
  [data]
  {:data     {:values data}
   ;:width    500
   :mark     "bar"
   :encoding {:x {:bin   true
                  ;:field "timestamp-added"
                  :field :vg-textchars}
              ;:type "temporal",
              ;:timeUnit "yearweek"
              ; https://vega.github.io/vega-lite/docs/timeunit.html
              ;:timeUnit "yearmonth"
              ;:timeUnit "yearmonthdate"}
              :y {:aggregate "count"}}}):1
