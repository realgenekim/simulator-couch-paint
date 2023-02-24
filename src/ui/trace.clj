(ns ui.trace
  (:require
   [genek.sim2 :as sim]
   [membrane.basic-components :as basic]
   [membrane.component :refer [defui defeffect make-app]]
   [membrane.ui :as ui]
   [membrane.skia :as skia]
   [membrane.skia.paragraph :as para]))


(def *sim-state genek.sim2/*state)

(def room-states
  [:waiting-for-movers1
   :removing-furniture
   :waiting-for-painters
   :painting
   :waiting-for-movers2
   :restoring-furniture
   :finished])

(defn vertical-packed
  "Returns a graphical elem of elems stacked on top of each other"
  [& elems]
  (let [elems (seq elems)
        first-elem (first elems)
        offset-y (+ (ui/height first-elem)
                    (ui/origin-y first-elem))]
    (when elems
      (loop [elems (next elems)
             offset-y offset-y
             group-elems [first-elem]]
        (if elems
          (let [elem (first elems)
                dy (+ (ui/height elem)
                      (ui/origin-y elem))]
            (recur
             (next elems)
             (+ offset-y dy)
             (conj group-elems
                   (ui/translate 0 offset-y
                                 elem))))
          group-elems)))))

(defn horizontal-packed
  "Returns a graphical elem of elems layed out next to eachother."
  [& elems]
  (let [elems (seq elems)
        first-elem (first elems)
        offset-x (+ (ui/width first-elem)
                    (ui/origin-x first-elem))]
    (when elems
      (loop [elems (next elems)
             offset-x offset-x
             group-elems [first-elem]]
        (if elems
          (let [elem (first elems)
                dx (+ (ui/width elem)
                      (ui/origin-x elem))]
            (recur
             (next elems)
             (+ offset-x dx)
             (conj group-elems
                   (ui/translate offset-x 0
                                 elem))))
          group-elems)))))

(def waiting?
  #{:waiting-for-movers1
    :waiting-for-painters
    :waiting-for-movers2})


(def state-colors
  (into {}
        (for [[i k] (map-indexed vector room-states)
              :let [a (float (/ i (dec (count room-states))))
                    base-color
                    (if (waiting? k)
                      [1 0 0]
                      [0 0 0])]]
          [k (conj base-color a)])))

(def block-width 1)
(def block-height 5)

(defn column [turn]
  (ui/vertical-layout
   (apply
    vertical-packed
    (for [[i room] (map-indexed vector (:rooms turn))
          :let [color (get state-colors (:state room))]]
      (ui/on
       :mouse-move
       (fn [_]
         [[::hover i (:state room)]])
       (ui/filled-rectangle color
                            block-width
                            block-height))))))

(defn trace [states]
  (ui/bordered 0
   (apply
    horizontal-packed
    (map column states))))

(defui trace-explore [{:keys [states hover-state]}]
  (ui/on
   ::hover
   (fn [i state]
     [[:set $hover-state [i state]]])
   [(when hover-state
      (let [[room-num room-state] hover-state]
        (ui/label (str room-num ": " room-state))))
    (ui/translate 0 20 (trace states))]
   
   ))


(comment
  (skia/save-image "trace.jpeg" (trace @*sim-state))

  (skia/run (make-app #'trace-explore {:states @*sim-state}))

  ,)
