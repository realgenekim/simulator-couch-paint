(ns ui.main
  (:require
    [membrane.skia :as skia]
    [membrane.component :refer [defui defeffect make-app]]
    [ui.gk-membrane-ui :as gm]))


(comment
  ; this allows getting away from global state, which we used for dev-view
  ;(def dev-app2 (make-app #'my-slider *app-state))
  ; Adrian: run these forms
  (gm/init-state! {})
  (gm/init-state! {:load-sim-state! true})
  (def dev-app2 (make-app #'gm/render-view gm/*app-state))
  (def w2 (skia/run dev-app2))
  0)

(comment
  (swap! *app-state
    assoc
    :frame 0
    :sim-state @*sim-state)

  (swap! *app-state
    update
    :sim-state
    #(subvec % 0 50))

  ; equiv to "pushing all the buttons in the window"
  (first
    (for [x (range 1000)
          y (range 1000)
          :let [intents (seq (ui/mouse-down (dev-view)
                               [x y]))]
          :when intents]
      intents))

  (ui/mouse-down (basic/number-slider {:num 0
                                       :min 0
                                       :max 100
                                       :integer? true})
    [10 10])



  0)

