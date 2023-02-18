(ns ui.membrane-ui
  (:require
    [membrane.ui :as ui]
    [membrane.skia :as skia]
    [membrane.basic-components :as basic]
    [membrane.component :refer
         [defui defeffect make-app]]))

; someday
; - learn about focus

(def *sim-state genek.sim2/*state)

; :frame: either frame number or :last-frame
(defonce *app-state (atom nil))

(defn init-state!
  []
  (reset! *app-state {:frame :last-frame}))

(comment
  (init-state!)
  0)

(defn selector
  [curr-page total-pages]
  (ui/on :key-press
    (fn [k]
      (println :selector :key-press k :type (type k))
      (case k
        "j" [[::next-frame]]
        "k" [[::prev-frame]]
        "$" [[::last-frame]]
        nil))

    (ui/horizontal-layout
      (ui/button "<<"
        (fn []
          [[::prev-frame]]))
      (ui/label (format "curr-page: %s, total-pages %s"
                  (str curr-page) (str total-pages)))
      (ui/button ">>"
        (fn []
          [[::next-frame]])))))


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

(defn render-view
  [sim-state *app-state]
  (let [state (last sim-state)]
    (ui/vertical-layout
      ; curr-page total-pages
      (selector (-> @*app-state :frame) (count sim-state))
      (turn sim-state)
      (rooms sim-state)
      (movers sim-state)
      (painters sim-state))))


(defn dev-view
  " helper: put anything you're working in here in dev
    (for prod app, it'll just be another view, composing all your components "
  []
  (let [states @*sim-state]
    (selector (-> @*app-state :frame) (count states))))
    ;(render-view @*sim-state *app-state)))

(comment
  (skia/run #'dev-view)
  ,)

(comment
  genek.sim2/*state

  (skia/run (fn [] (ui/label "hi"))))