(ns ui.membrane-ui
  (:require
    [membrane.ui :as ui]
    [membrane.skia :as skia]
    [membrane.basic-components :as basic]
    [membrane.component :refer
         [defui defeffect make-app]]))


(skia/run (fn [] (ui/label "hi")))