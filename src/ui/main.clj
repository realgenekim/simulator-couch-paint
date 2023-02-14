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

(def app
  "Main app definition."
  (ui/default-theme ; we must wrap our app in a theme
    {}
    ;; just some random stuff
    (ui/column
      (ui/row
        (ui/label "hi"))
      ;(ui/row
      ;  (ui/label (str (-> @sim/*state last :rooms vec))))
      (for [r (-> @sim/*state last :rooms)]
        (ui/row
          (ui/rect
            (paint/stroke 0xFFCCCCCC 2)
            (ui/width 500
              (ui/height 100
                (ui/valign 0.5
                  (ui/label
                      (format "Room %d: %s"
                        (-> r :id)
                        (-> r :state))))))))))))
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

(comment
  (-main)
  (state/redraw!)
  0)
