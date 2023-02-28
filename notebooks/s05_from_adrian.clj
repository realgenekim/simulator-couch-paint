(ns s05-from-adrian
  (:require
    ;[membrane.skia :as skia]
    ;[membrane.component :refer [defui defeffect make-app]]
    [membrane.ui :as ui])
    ;[ui.gk-membrane-ui :as gm]))
  (:import
    (com.kitfox.svg SVGUniverse)
    (java.awt Graphics2D RenderingHints)
    (java.awt.image BufferedImage)
    (java.io File)
    (javax.imageio ImageIO)))


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


(comment
  0
  (def bi (BufferedImage. 100 100 BufferedImage/TYPE_INT_ARGB))
  (System/setProperty "java.awt.headless" "true")
  (def g (.createGraphics bi))
  (.setColor g Color/BLACK)
  (.drawString g "hello" 0 50))

; Adrian, got it working at 6m!
(skia/run (constantly
            (ui/vertical-layout
              (ui/label "hello")
              #_(ui/image
                   (get-image-bytes bi))
              (skia/svg
                (slurp (File. "furniture.svg")))
              #_(ui/image  "furniture.png" [nil 120]))))

(comment

  (skia/run
    (constantly
      (skia/svg
        (slurp "https://www1.plurib.us/1shot/2007/spring_tree/spring_tree_final.svg"))))

  ; get rid of all this below

  (def svgu (SVGUniverse.))
  (def svgd (.getDiagram svgu (.toURI (File. "furniture.svg"))))
  ;(.loadSVG svgu (clojure.java.io/input-stream "furniture.svg"))

  (def bi (BufferedImage. 1200 240 BufferedImage/TYPE_INT_ARGB))
  ; https://stackoverflow.com/questions/46793769/bufferedimage-causes-a-program-freeze-on-macos-but-not-on-windows
  (def g2d (.createGraphics bi))
  ; ^^^ hangs
  (type g2d)
  (.setRenderingHint g2d RenderingHints/KEY_ANTIALIASING, RenderingHints/VALUE_ANTIALIAS_ON)

  (type svgd)
  (.render svgd g2d)
  (ImageIO/write bi "PNG" (File. "furniture.png"))

  0)