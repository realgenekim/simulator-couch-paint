(ns ui.membrane-ui
  (:require
    [genek.entities :as e]
    [genek.sim2 :as sim]
    [membrane.skia.paragraph :as para]
    [membrane.ui :as ui]
    [membrane.skia :as skia]
    [taoensso.timbre :as log]
    [membrane.basic-components :as basic]
    [membrane.component :refer
     [defui defeffect make-app]]))


; someday
; - learn about focus

; next
; X  I’ll have ability to nav through all the frames
;    - prev/next
;    - last frame
;    - handle inc/dec :last-frame
; X and a way to animate all the frames.
; X show all stages [:start :waiting-for-movers1 ...]
;   X highlight which stage we're in (RED)
; - create pane on the left, with buttons: "start FIFO" "start LIFO"
; - slider
; someday
; - left pane
; X take side effects out of event handler

; Adrian, to run:
; go into ns notebooks/s02-recursive-search.clj, and load the namespace -- it will
; create the state, which is about 500 frames
; then in this namespace, run
; (init-state!)
; (def w (skia/run #'dev-view))

(def *sim-state genek.sim2/*state)

; :frame: either frame number or :last-frame
(defonce *app-state (atom nil))

(defn init-state!
  []
  ;(reset! *app-state {:frame :last-frame})
  (log/warn :init-state! :running-simulator)

  (sim/init-state!)
  (sim/simulate-until-done (-> @sim/*state last) {:maxturns 500})

  (log/warn :init-state! :updating-atom)
  (swap! *app-state
     assoc
     :frame 0
     :sim-state @*sim-state
     :*sim-state *sim-state)

  nil)


(comment
  (init-state!)
  0)

(defn next-frame!
  [curr-page total-pages]
  ; cases:
  ;  handle overflow:  more than total-pages
  ;  handle :last-frame: leave unchanged
  (cond
    (= :last-frame curr-page) nil
    (= (dec total-pages) curr-page) nil
    :else (swap! *app-state update-in [:frame] inc)))

(defn prev-frame!
  [curr-page total-pages]
  ; cases:
  ;  handle underflow: if 0, leave at zero
  ;  handle :last-frame: (dec total-pages)
  (log/warn :prev-frame! :curr-page curr-page :total-pages total-pages)
  (case curr-page
    0 nil
    :last-frame (swap! *app-state assoc-in [:frame] (dec total-pages))
    (swap! *app-state update-in [:frame] dec)))

(defn last-frame!
  []
  (swap! *app-state assoc-in [:frame] :last-frame))

(defn first-frame!
  []
  (swap! *app-state assoc-in [:frame] 0))

(defn animate-all-frames!
  " start from zero, and then on a timer, advance frames to the end
    run in a future; pass first frame, recurse to end (total-pages)

    input: framenum"
  [framenum total-pages]
  (first-frame!)
  (loop [framenum framenum
         total-page total-pages]
    ; termination case
    (log/debug :animate-all-frames!/entering :framenum framenum :total-pages total-pages)
    (if (< framenum total-pages)
      (do
        (Thread/sleep 50)
        (next-frame! framenum total-pages)
        ; see if we can force repaint
        (if-let [w (resolve `w2)]
          ((:membrane.skia/repaint @w)))
        (recur (inc framenum) total-pages)))))

(defeffect ::prev-frame
  [curr-page total-pages]
  (prev-frame! curr-page total-pages))

(defn selector
  [curr-page total-pages]
  (ui/on
    ::next-frame (fn []
                   (log/warn ::next-frame)
                   (next-frame! curr-page total-pages)
                   nil)
    ::prev-frame (fn []
                   (log/warn ::prev-frame)
                   [[::prev-frame curr-page total-pages]])
    ::last-frame (fn []
                   (log/warn ::last-frame)
                   (last-frame!)
                   nil)
    ::first-frame (fn []
                    (log/warn ::first-frame)
                    (first-frame!)
                    nil)
    ::animate-all-frames (fn []
                           (log/warn ::animate-all-frames)
                           ; run in future
                           (future
                             (animate-all-frames! 0 total-pages))
                           nil)
    :key-press (fn [k]
                 (log/warn :selector :key-press k :type (type k))
                 (case k
                   "j" [[::next-frame]]
                   "k" [[::prev-frame]]
                   "$" [[::last-frame]]
                   ;["^" "0"] [[::first-frame]]
                   "^" [[::first-frame]]
                   "0" [[::first-frame]]
                   "A" [[::animate-all-frames]]
                   "a" [[::animate-all-frames]]
                   nil))

    (ui/horizontal-layout
      (ui/button "<<"
        (fn []
          [[::prev-frame]]))
      (ui/label (format "curr-page: %s, total-pages %s"
                  (str curr-page) (str total-pages)))
      (ui/button ">>"
        (fn []
          [[::next-frame]]))
      (ui/button "Animate!"
        (fn []
          [[::animate-all-frames]])))))


(defn turn
  [state]
  (ui/vertical-layout
    (ui/horizontal-layout
      (ui/label (format "Turn: %d"
                  (-> state :turn))))
    (ui/spacer 25)))

(def states-text { :initial "start"
                  :waiting-for-movers1 "wait"
                  :removing-furniture [{:text "🛋 "
                                        :style #:text-style {:font-size 9}}
                                       "moving"]
                  :waiting-for-painters "wait"
                  :painting "painting"
                  :waiting-for-movers2 "wait"
                  :restoring-furniture "moving"
                  :finished "done!"})

(comment
  (get states-text :initial)
  0)

(defn make-red [text]
  (cond
    (string? text)
    {:text text
     :style #:text-style {:color [1 0 0]}}

    (map? text)
    (assoc-in text [:style :text-style/color] [1 0 0])

    :else
    (map make-red text)))

(defn room-state
  " show all room states, and highlight the current one
    [:initial   :waiting-for-movers1   :removing-furniture   :waiting-for-painters   :painting   :waiting-for-movers2   :restoring-furniture   :finished])"
  [room]
  (ui/translate
   0 3
   (para/paragraph
    (interpose
     " "
     (for [st e/room-states]
       (if (not= st (:state room))
         (get states-text st)
         (make-red (get states-text st))))))))

(defn time-remaining-bar
  " just time remaining as string of chars "
  [n]
  (ui/label
     (apply str (repeat n "*"))))

(defn workers-present
  " show any movers/painters in room"
  [r movers painters]
  (let [rid (:id r)
        workers (->> (concat movers painters)
                  (filter #(= rid (:at-room %)))
                  vec)
        msg     (->> workers
                  (mapv (fn [x]
                          (format "%s %d" (:role x) (:id x)))))]
    (ui/horizontal-layout
      ;(ui/label (str workers))
      (ui/with-color [0 0 1]
        (if-not (empty? msg)
          (ui/label msg)
          (ui/label "  "))))))

(defn room
  [r movers painters]
  (ui/vertical-layout
    (ui/horizontal-layout
      (ui/label (format "Room %d:"
                  (-> r :id)))
      (room-state r))

    (ui/horizontal-layout
      (ui/label (format "                :moving1-time-remaining: %d"
                  (-> r :moving1-time-remaining)))
      (time-remaining-bar (-> r :moving1-time-remaining)))

    (ui/horizontal-layout
      (ui/label (format "                :painting-time-remaining: %d"
                  (-> r :painting-time-remaining)))
      (time-remaining-bar (-> r :painting-time-remaining)))

    (ui/horizontal-layout
      (ui/label (format "                :moving2-time-remaining: %d"
                  (-> r :moving2-time-remaining)))
      (time-remaining-bar (-> r :moving2-time-remaining)))

    (ui/vertical-layout
      #_(ui/spacer 70 20)
      (ui/horizontal-layout
        (ui/spacer 70 10)
        (workers-present r movers painters)))
    (ui/spacer 10)))


(defn rooms
  " main view: will show all details of room, as well as any movers/painters present "
  [state]
  (let [{:keys [rooms movers painters]} state]
    (apply
      ui/vertical-layout
      (for [r rooms]
        (let [relem (ui/padding
                     4
                     (room r movers painters))
              bounds (ui/bounds relem)]
          [(ui/with-style :membrane.ui/style-stroke
             (ui/rectangle (first bounds) (- (second bounds) 2)))
           relem])))))

(defn movers
  [state]
  (apply
    ui/vertical-layout
    (interpose (ui/spacer 10)
      (for [r (-> state :movers)]
        (let [roomnum (-> r :at-room)]
          (ui/label (format "Mover %d -- In Room: %s"
                      (-> r :id)
                      (if roomnum
                        (str (-> r :at-room))
                        "---"))))))))

(defn painters
  [state]
  (apply
    ui/vertical-layout
    (interpose (ui/spacer 10)
      (for [r (-> state :painters)]
        (let [roomnum (-> r :at-room)]
          (ui/label (format "Painter %d -- In Room: %s"
                      (-> r :id)
                      (if roomnum
                        (str (-> r :at-room))
                        "---"))))))))

(defn get-frame
  " get frame, and prevent overflow "
  [n frames]
  ;(log/warn :get-frame :n n :maxn maxn)
  ;(log/warn :get-frame :n n :type (type frames))
  (let [maxn (count frames)]
    (if (>= n maxn)
      (last frames)
      (nth frames n))))

(comment
  (get-frame 100 @*sim-state)
  0)

(defn parse-framenum
  " handle :last-frame: return an int "
  [framenum sim-state]
  (case framenum
    :last-frame (count sim-state)
    framenum))


(defui my-slider
  [{:keys [frame sim-state]}]
  (ui/vertical-layout
    ;(ui/label "hello!")
    ;(ui/label (str frame))

    ; handle :last-frame
    (let [fnum (parse-framenum frame sim-state)]
      (basic/number-slider {:num fnum
                            :min 0
                            :max (dec (count sim-state))
                            :integer? true}))))


(defui outer-pane
  [{:keys [view]}]
  (ui/horizontal-layout
    (ui/horizontal-layout
      [
       (ui/spacer 100)
       (ui/vertical-layout
         (basic/button {:text     "Initialize"
                        :on-click #(do
                                     (log/warn :outer-pane :click)
                                     (init-state!))}))])

    (ui/horizontal-layout
      ;[(ui/spacer 100)
      ; (ui/label "hello2")
      ; (ui/spacer 100)
       view)))

(comment
  (def w3 (skia/run (make-app #'outer-pane {})))
  0)


(defui render-view
  [{:keys [frame sim-state *sim-state]
    :as   m}]
  (let [
        state    (case frame
                   :last-frame (last sim-state)
                   (get-frame frame sim-state))]
    (outer-pane {:view
                 (ui/vertical-layout
                   ; curr-page total-pages
                   (my-slider {:frame     frame
                               :sim-state sim-state})
                   (selector frame (count sim-state))
                   (turn state)
                   (rooms state)
                   (movers state)
                   (painters state))})))

(comment
  ; this allows getting away from global state, which we used for dev-view
  ;(def dev-app2 (make-app #'my-slider *app-state))
  (init-state!)
  (def dev-app2 (make-app #'render-view *app-state))
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

(defn dev-view
  " helper: put anything you're working in here in dev
    (for prod app, it'll just be another view, composing all your components "
  []
  (let [states @*sim-state]
    ;(selector (-> @*app-state :frame) (count states))
    (render-view @*sim-state *app-state)))



(comment
  (skia/run #'dev-view)

  (init-state!)
  (def w (skia/run #'dev-view))
  (def w1 (skia/run #'dev-view))
  ((:membrane.skia/repaint w))
  @*app-state

  (dev-view)
  (skia/save-image "/tmp/devvew.png" (dev-view))

  0)


(comment
  genek.sim2/*state
  (skia/run (fn [] (ui/label "hi"))))