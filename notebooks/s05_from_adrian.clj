(ns s05-from-adrian
  (:require
    ;[membrane.skia :as skia]
    ;[membrane.component :refer [defui defeffect make-app]]
    [membrane.ui :as ui]))
    ;[ui.gk-membrane-ui :as gm]))


(import 'java.awt.image.BufferedImage
  'javax.imageio.ImageIO
  'java.awt.Color
  'java.io.ByteArrayOutputStream)
(require '[membrane.skia :as skia])

(defn get-image [fname]
  (with-open [is (clojure.java.io/input-stream fname)]
    (let [image-stream (ImageIO/createImageInputStream is)
          buffered-image (ImageIO/read image-stream)]
      buffered-image)))

(defn get-image-bytes [img]
  (let [baos (ByteArrayOutputStream.)]
    (ImageIO/write ^BufferedImage img "png" baos)
    (.toByteArray baos)))

(def bi (BufferedImage. 100 100 BufferedImage/TYPE_INT_ARGB))

(System/setProperty "java.awt.headless" "true")
(def g (.createGraphics bi))
(.setColor g Color/BLACK)
(.drawString g "hello" 0 20)

(skia/run (constantly
            (ui/image
               (get-image-bytes bi))))