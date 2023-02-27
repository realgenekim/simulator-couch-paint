(ns s03-vega-graphs
  (:require
    [nextjournal.clerk :as clerk]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [genek.my-darkstar :as darkstar]
    [clojure.data.json :as json]
    [clojure.java.shell :as shell]
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
  {"$schema" "https://vega.github.io/schema/vega-lite/v5.json"
   :data {:values vs}
   :mark {:type :line}
   :encoding {:x {:field :turn
                  :type :quantitative}
              :y {:field :furnture-in-storage
                  :type :quantitative}}})

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
              :y {:aggregate "count"}}})



(-> @sim/*state second)
(-> @sim/*state second :furniture :in-storage)


(defn points
  [states]
  (->> states
    ;identity
     (map (fn [s]
            {:turn                (-> s :turn)
             :furnture-in-storage (or
                                    (-> s :furniture :in-storage)
                                    0)}))))

(points @sim/*state)

(vega-plot1 (points @sim/*state))

(def vg (merge {:width 400
                :height 100}
          (vega-plot1  (points @sim/*state))))

(clerk/vl vg)

(def vg-json
  (->> vg
    (json/write-str)))
    ;darkstar/vega-spec->svg))

(->> vg-json
  (spit "furniture.json"))

(def vg-svg
  (->> vg-json
    darkstar/vega-lite-spec->svg))

(->> vg-svg
  (spit "furniture.svg"))

; convert to png







(comment
  (->> (slurp "vega-example.json")
    darkstar/vega-spec->svg
    (spit "vg-example.svg")))

(def rsvg-width 600)

(defn convert-svg-to-png
  []
  (let [svgfiles ["furniture.svg"]
        dir "./"]
    ;(fs/glob dir "*.svg")]

    (doseq [svg (->> svgfiles
                  (map str)
                  sort)]
      (do
        (println :svg (str svg))
        (let [fname   (-> (str svg)
                        ;(clojure.string/replace #"svg$" "png")
                        (clojure.string/replace #"^.*/" ""))
              _       (println :fname fname)
              pngname (clojure.string/replace fname #"svg$" "png")
              cmd     (format "/usr/local/bin/rsvg-convert -w %d --keep-aspect-ratio --background-color=white %s"
                        rsvg-width fname)
              cmdvec  (conj (clojure.string/split cmd #"\s+")
                        :out-enc :bytes)
              ;_       (println cmdvec)
              sh-out  (shell/with-sh-dir dir
                        (apply shell/sh cmdvec))]

          (def XXX sh-out)
          (shell/with-sh-dir dir
            (clojure.java.io/copy
              (:out sh-out)
              (java.io.File. (str dir "/" pngname))))
          (println :newnamme fname)
          ;(println :cmd cmd)

          0)))))

(convert-svg-to-png)