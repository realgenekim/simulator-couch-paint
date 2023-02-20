(ns ui.gk-membrane-helpers
  (:require
    [membrane.skia.paragraph :as para]))




; https://vega.github.io/vega/docs/schemes/

; helpful colors

; 240, 2, 127: red
; 56, 108, 176: blue

(defn rgb255vec>mrgbvec
  " take 3-tuple of 0-255, turn into 3-tuple of 0-1 float (e.g., [240, 2, 127] "
  [v]
  ;(println v)
  (->> v
    (mapv (fn [n]
            ;(println "-- " n)
            (-> (/ n 255)
              double)))))

(comment
  (rgb255vec>mrgbvec [240, 2, 127])
  (rgb255vec>mrgbvec [0, 255, 128])
  (-> (/ 128 255) float)
  0)

(def accent-red (rgb255vec>mrgbvec [240, 2, 127]))
(def accent-blue (rgb255vec>mrgbvec [56, 108, 176]))
(def accent-brown (rgb255vec>mrgbvec [191, 91, 23]))
(def accent-green (rgb255vec>mrgbvec [127, 201, 127]))
(def accent-purple (rgb255vec>mrgbvec [190, 174, 212]))

(def set1-purple (rgb255vec>mrgbvec [152, 78, 163]))
(def set1-green (rgb255vec>mrgbvec [77, 175, 74]))
(def set1-red (rgb255vec>mrgbvec [228, 26, 28]))


(defn my-label
  " behaves like like ui/label, but use our fonts, sizes, etc. "
  [s]
  (para/paragraph
    {:text  s
     :style #:text-style {:font-size 13}}))