(ns ui.membrane-ui
  (:require
    [membrane.ui :as ui]
    [membrane.skia :as skia]
    [membrane.basic-components :as basic]
    [membrane.component :refer
         [defui defeffect make-app]]))

(def *mystate genek.sim2/*state)

(defn turn
  [states]
  (ui/vertical-layout
    (ui/horizontal-layout
      (ui/label (format "Turn: %d"
                  (-> states last :turn))))
    (ui/spacer 25)))


(defn rooms
  [states]
  (apply
    ui/vertical-layout
    (for [r (-> states last :rooms)]
      (ui/vertical-layout
        (ui/label (format "Room %d: %s"
                    (-> r :id)
                    (-> r :state)))
        (ui/label (format "    :moving1-time-remaining: %d"
                    (-> r :moving1-time-remaining)))
        (ui/label (format "    :painting-time-remaining: %d"
                    (-> r :painting-time-remaining)))
        (ui/label (format "    :moving2-time-remaining: %d"
                    (-> r :moving2-time-remaining)))
        (ui/spacer 25)))))

(defn movers
  [states]
  (apply
    ui/vertical-layout
    (interpose (ui/spacer 10)
      (for [r (-> states last :movers)]
        (let [roomnum (-> r :at-room)]
          (ui/label (format "Mover %d -- In Room: %s"
                      (-> r :id)
                      (if roomnum
                        (str (-> r :at-room))
                        "---"))))))))

(defn painters
  [states]
  (apply
    ui/vertical-layout
    (interpose (ui/spacer 10)
      (for [r (-> states last :painters)]
        (let [roomnum (-> r :at-room)]
          (ui/label (format "Painter %d -- In Room: %s"
                      (-> r :id)
                      (if roomnum
                        (str (-> r :at-room))
                        "---"))))))))


(defn dev-view []
  " helper: put anything you're working in here in dev
    (for prod app, it'll just be another view, composing all your components "
  (let [states @*mystate]
    (ui/vertical-layout
      (turn states)
      (rooms states)
      (movers states)
      (painters states))))

(comment
  (skia/run #'dev-view)
  ,)

(comment
  genek.sim2/*state

  (skia/run (fn [] (ui/label "hi"))))