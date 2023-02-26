(ns s03-vega-graphs
  (:require
    [nextjournal.clerk :as clerk]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [genek.sim2 :as sim]))


(-> @sim/*leaf-counter)

(-> @sim/*state count)

;
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
(defn vega-plot1
  [vs]
  {:data
   {:values vs}
   :mark {:type :line}
   :encoding {:x {:field :turn
                  :type :quantitative}
              :y {:field :furnture-in-storage
                  :type :quantitative}}})



(-> @sim/*state second)
(-> @sim/*state second :furniture :in-storage)


(defn points
  [states]
  (->> states
     (map (fn [s]
            {:turn (-> s :turn)
             :furnture-in-storage (-> s :furniture :in-storage)}))))

(points @sim/*state)

(vega-plot1 (points @sim/*state))

(clerk/vl
  (merge {:width 400}
    (vega-plot1  (points @sim/*state))))

