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
  (ui/horizontal-layout
    (ui/label (format "Turn: %d"
                (-> states last :turn)))))

(defn dev-view []
  " helper: put anything you're working in here in dev
    (for prod app, it'll just be another view, composing all your components "
  (let [states @*mystate]
    (turn states)))

(comment
  (skia/run #'dev-view)
  ,)

(comment
  genek.sim2/*state

  (skia/run (fn [] (ui/label "hi"))))