(ns s03-vega-graphs
  (:require
    [batik.rasterize :as b]
    [clojure.data.json :as json]
    [clojure.java.shell :as shell]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [genek.my-darkstar :as darkstar]
    [genek.sim2 :as sim]
    [genek.vega :as gv]
    [membrane.skia :as skia]
    [membrane.ui :as ui]
    [nextjournal.clerk :as clerk])
  (:import
    (java.io File)))


(-> @sim/*leaf-counter)

(-> @sim/*state count)




(-> @sim/*state second)
(-> @sim/*state second :furniture :in-storage)




(gv/points @sim/*state)

(gv/vega-plot-furniture-vs-time (gv/points @sim/*state))

(def vg (gv/states>furniture-plot @sim/*state))


(def vg-svg2 (->> (gv/vega-plot-furniture-vs-time (gv/points @sim/*state))
               (gv/vega>svg)))

(clerk/html vg-svg2)

      ;(deep-merge {:mark {:point "true"}})
                          ;:color "yellow"}})

(comment
  (->>
    (gv/points @sim/*state)
    (take 10)
    (gv/vega-plot-furniture-vs-time-highlight-turn 5))
  0)

; ## with color highlight 2
(clerk/vl
  (->>
    (gv/points @sim/*state)
    ;(take 10)
    (gv/vega-plot-furniture-vs-time-highlight-turn 7)))

(def vg-svg3 (->> (gv/vega-plot-furniture-vs-time (gv/points @sim/*state))
               (json/write-str)
               darkstar/vega-lite-spec->svg))

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



  0)







(comment
  (clojure.java.io/input-stream)
  (->> (slurp "vega-example.json")
    darkstar/vega-spec->svg
    (spit "vg-example.svg")))

(def rsvg-width 600)


(defn dev-view
  []
  (ui/vertical-layout
    (ui/label "Adrian, here are 3 SVG graphs from vega-lite!")
    #_(ui/image
        (get-image-bytes bi))
    (skia/svg
      (->>
        (gv/points @sim/*state)
        ;(take 10)
        (vega-plot-furniture-vs-time-highlight-turn 10)
        (gv/vega>svg)))
    (skia/svg vg-svg)
    (skia/svg vg-svg2)
    (skia/svg vg-svg3)

    ;(skia/svg (slurp (File. "furniture.svg")))
    #_(ui/image  "furniture.png" [nil 120])))


; ## to PNG

(comment
  ;(def w2 (skia/run dev-view))
  (def w (skia/run #'dev-view))

  (->>
    (gv/points @sim/*state)
    (take 10)
    (gv/vega-plot-furniture-vs-time-highlight-turn 10))
    ;(gv/vega>svg))

  (time
    (def retval
      (doall
        (for [r (range 1)]
          (->> (gv/states>furniture-plot @sim/*state)
            (gv/vega>svg))))))
  ;(b/parse-svg-string vg-svg)
  ;(b/render-svg-string vg-svg nil {:type :png})
  ;(b/parse-svg-string vg-svg "furniture.png")
  ;(b/render-svg-uri "furniture.svg" "furniture.png")
  ; Adrian, 14m: I've been struggling to get the dev-view app to refresh;
  ; how do I get it to load without restarting REPL?
  ; Adrian, got it working at about 18m mark!

  0)