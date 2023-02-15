(ns ui.main
;(ns town.lilac.humble.app.main
  "The main app namespace.
  Responsible for initializing the window and app state when the app starts."
  (:require
    [io.github.humbleui.ui :as ui]
    [io.github.humbleui.paint :as paint]
    ;; [io.github.humbleui.window :as window]
    ;[town.lilac.humble.app.state :as state]
    [ui.state :as state]
    [genek.sim2 :as sim]
    [genek.utils :as utils])
  (:import
    [io.github.humbleui.skija Color ColorSpace]
    [io.github.humbleui.jwm Window]
    [io.github.humbleui.jwm.skija LayerMetalSkija]))

; https://github.com/HumbleUI/HumbleUI/blob/main/dev/examples/wordle.clj

(defn rooms
  []
  (for [r (-> @sim/*state last :rooms)]
    (ui/row
      (ui/rect
        (paint/stroke 0xFFCCCCCC 4)
        (ui/width 500
          (ui/height 100
            (ui/valign 0.5
              (ui/column
                (ui/row
                  (ui/label
                    (format "Room %d: %s"
                      (-> r :id)
                      (-> r :state))))
                (ui/gap 0 10)
                (ui/row
                  (ui/label
                    (format "    :moving1-time-remaining: %d"
                      (-> r :moving1-time-remaining))))
                (ui/gap 0 10)
                (ui/row
                  (ui/label
                    (format "    :painting-time-remaining: %d"
                      (-> r :painting-time-remaining))))
                (ui/gap 0 10)
                (ui/row
                  (ui/label
                    (format "    :moving2-time-remaining: %d"
                      (-> r :moving2-time-remaining))))))
            #_(ui/label
                (format "    x"))))))))

(defn movers
  []
  (for [r (-> @sim/*state last :movers)]
    (ui/row
      (ui/rect
        (paint/stroke 0xFFCCCCCC 4)
        (ui/width 500
          (ui/height 40
            (ui/valign 0.5
              (ui/column
                (ui/row
                  (ui/label
                    (format "Mover %d -- In Room: %s"
                      (-> r :id)
                      (str (-> r :at-room)))))))))))))

(defn painters
  []
  (for [r (-> @sim/*state last :painters)]
    (ui/row
      (ui/rect
        (paint/stroke 0xFFCCCCCC 4)
        (ui/width 500
          (ui/height 40
            (ui/valign 0.5
              (ui/column
                (ui/row
                  (ui/label
                    (format "Painter %d -- In Room: %s"
                      (-> r :id)
                      (str (-> r :at-room)))))))))))))

(defn turn
  []
  (let [s (-> @sim/*state last)]
    (ui/row
      (ui/rect
        (paint/stroke 0xFFCCCCCC 4)
        (ui/width 500
          (ui/height 40
            (ui/valign 0.5
              (ui/column
                (ui/row
                  (ui/label
                    (format "Turn %d"
                      (-> s :turn))))))))))))


(def app
  "Main app definition."
  (ui/default-theme ; we must wrap our app in a theme
    {}
    ;; just some random stuff
    (ui/column
      ;(ui/row
      ;  (ui/label "hi")
      ;(ui/row
      ;  (ui/label (str (-> @sim/*state last :rooms vec))))
      (turn)
      (rooms)
      (movers)
      (painters))))
;(utils/pp-str (-> r)))))))))

;; reset current app state on eval of this ns
(reset! state/*app app)

(defn -main
  "Run once on app start, starting the humble app."
  [& args]
  (ui/start-app!
    (reset! state/*window
       (ui/window
        {:title    "Editor"
         :bg-color 0xFFFFFFFF}
        state/*app)))
  (state/redraw!))

(state/redraw!)

(add-watch sim/*state :ui-watcher
  (fn [key atom old-state new-state]
    (state/redraw!)))

(comment
  (-main)
  (state/redraw!)
  0)
