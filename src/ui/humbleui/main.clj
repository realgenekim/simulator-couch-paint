;(ns ui.main
;;(ns town.lilac.humble.app.main
;  "The main app namespace.
;  Responsible for initializing the window and app state when the app starts."
;  (:require
;    [io.github.humbleui.ui :as ui]
;    [io.github.humbleui.paint :as paint]
;    ;; [io.github.humbleui.window :as window]
;    ;[town.lilac.humble.app.state :as state]
;    [ui.state :as state]
;    [genek.sim2 :as sim]
;    [genek.utils :as utils])
;  (:import
;    [io.github.humbleui.skija Color ColorSpace]
;    [io.github.humbleui.jwm Window]
;    [io.github.humbleui.jwm.skija LayerMetalSkija]))
;
;; https://github.com/HumbleUI/HumbleUI/blob/main/dev/examples/wordle.clj
;
;; -1 is last
;(def *config (atom {:max   (-> @sim/*state count)
;                    :value (-> @sim/*state count dec)}))
;
;(defn get-state
;  " get state turn, based on config "
;  [slider]
;  (let [turn (-> slider :value dec)
;        state (-> @sim/*state
;                (nth turn))]
;    (println :get-state :turn turn :state state)
;    state))
;
;
;(defn rooms
;  [slider]
;  (let [state (get-state slider)]
;    (for [r (-> state :rooms)]
;      (ui/row
;        (ui/rect
;          (paint/stroke 0xFFCCCCCC 4)
;          (ui/width 500
;            (ui/height 100
;              (ui/valign 0.5
;                (ui/column
;                  (ui/row
;                    (ui/label
;                      (format "Room %d: %s"
;                        (-> r :id)
;                        (-> r :state))))
;                  (ui/gap 0 10)
;                  (ui/row
;                    (ui/label
;                      (format "    :moving1-time-remaining: %d"
;                        (-> r :moving1-time-remaining))))
;                  (ui/gap 0 10)
;                  (ui/row
;                    (ui/label
;                      (format "    :painting-time-remaining: %d"
;                        (-> r :painting-time-remaining))))
;                  (ui/gap 0 10)
;                  (ui/row
;                    (ui/label
;                      (format "    :moving2-time-remaining: %d"
;                        (-> r :moving2-time-remaining))))))
;              #_(ui/label
;                  (format "    x")))))))))
;
;(defn movers
;  [slider]
;  (let [state (get-state slider)]
;    (for [r (-> state :movers)]
;      (ui/row
;        (ui/rect
;          (paint/stroke 0xFFCCCCCC 4)
;          (ui/width 500
;            (ui/height 40
;              (ui/valign 0.5
;                (ui/column
;                  (ui/row
;                    (ui/label
;                      (format "Mover %d -- In Room: %s"
;                        (-> r :id)
;                        (str (-> r :at-room))))))))))))))
;
;(defn painters
;  [slider]
;  (let [state (get-state slider)]
;    (for [r (-> state :painters)]
;      (ui/row
;        (ui/rect
;          (paint/stroke 0xFFCCCCCC 4)
;          (ui/width 500
;            (ui/height 40
;              (ui/valign 0.5
;                (ui/column
;                  (ui/row
;                    (ui/label
;                      (format "Painter %d -- In Room: %s"
;                        (-> r :id)
;                        (str (-> r :at-room))))))))))))))
;
;(defn turn
;  [*slider]
;  (let [state (get-state @*slider)]
;    (ui/dynamic ctx
;      [value @*slider]
;      (ui/row
;        (ui/rect
;          (paint/stroke 0xFFCCCCCC 4)
;          (ui/width 500
;            (ui/height 40
;              (ui/valign 0.5
;                (ui/column
;                  (ui/row
;                    (ui/label
;                      (format "Turn %d of %d"
;                        (-> state :turn)
;                        (-> @sim/*state count)))))))))))))
;
;(defn slider
;  [*config]
;  (ui/row
;    (ui/rect
;      (paint/stroke 0xFFCCCCCC 4)
;      (ui/width 500
;        (ui/height 40
;          (ui/valign 0.5
;            (ui/column
;              (ui/label "Turn:")
;              (ui/slider *config))))))))
;
;
;
;
;;"Main app definition."
;(defn reload!
;  []
;  (def app
;    (ui/default-theme ; we must wrap our app in a theme
;      {}
;      ;; just some random stuff
;      (ui/column
;        ;(ui/row
;        (slider *config)
;        (turn *config)
;        (rooms @*config)
;        (movers @*config)
;        (painters @*config))))
;  (reset! state/*app app)
;  (state/redraw!)
;
;  nil)
;;(utils/pp-str (-> r)))))))))
;
;(reload!)
;
;;; reset current app state on eval of this ns
;(reset! state/*app app)
;
;(defn set-watcher!
;  []
;  (add-watch sim/*state :ui-watcher
;    (fn [key atom old-state new-state]
;      (println :add-watch "*** firing! turn: "
;        (-> new-state last :turn))
;      ;(println new-state)
;      ;(reset! state/*app app)
;      (Thread/sleep 250)
;      (reload!)))
;
;  #_(add-watch *config :slider
;      (fn [key atom old-state new-state]
;        ;(println :add-watch "*** firing! turn: " @*config)
;        (reload!))))
;
;(comment
;  (set-watcher!)
;  (remove-watch *config :slider)
;  0)
;
;(defn -main
;  "Run once on app start, starting the humble app."
;  [& args]
;  (ui/start-app!
;    (reset! state/*window
;       (ui/window
;        {:title    "Editor"
;         :bg-color 0xFFFFFFFF}
;        state/*app)))
;  (state/redraw!)
;  (set-watcher!))
;
;(state/redraw!)
;
;(comment
;  (-main)
;  (reload!)
;  (state/redraw!)
;  (set-watcher!)
;  (count @sim/*state)
;  0)
;