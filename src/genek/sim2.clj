(ns genek.sim2
  (:require
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]))

(def room-states
  [:initial
   :waiting-for-movers1
   :removing-furniture
   :waiting-for-painters
   :painting
   :waiting-for-movers2
   :restoring-furniture
   :finished])

(def MOVING1-OP-TURNS 10)
(def PAINTING-OP-TURNS 50)
(def MOVING2-OP-TURNS 10)


(>defn create-room
  [id] [integer? => map?]
  {:id id
   :state (first room-states)
   :moving1-time-remaining MOVING1-OP-TURNS
   :painting-time-remaining PAINTING-OP-TURNS
   :moving2-time-remaining MOVING2-OP-TURNS})


;(defn insert-entity!
;  [sess a v]
;  (let [id @*latest-id]
;    (swap! *latest-id inc)
;    (println :insert-entity id a v)
;
;    (swap! *session
;      (fn [session]
;        (-> session
;          (o/insert id
;            {::role :supervisor
;             ::name v
;             ::x 100
;             ::y 100})
;          (o/fire-rules))))))
;
;(defn add-supervisors
;  " add steve and gene"
;  []
;  (insert-entity! *session nil "steve")
;  (insert-entity! *session nil "gene"))
;
;(add-supervisors)
;
;(defn add-movers
;  []
;  (let [eavs (for [i (range 3)]
;               [(+ 10 i) {::role :mover
;                          ::name (format "mover-%d" i)
;                          ::x    100
;                          ::y    100}])]
;    (doall
;      (for [[i eav] eavs]
;        (do
;          (println i eav)
;          (swap! *session
;            (fn [session]
;              (-> session
;                (o/insert i eav)
;                (o/fire-rules))))))))
;  0)
;
;(defn add-painters
;  []
;  (let [eavs (for [i (range 3)]
;               [(+ 20 i) {::role :painter
;                          ::name (format "painter-%d" i)
;                          ::x    100
;                          ::y    100}])]
;    (doall
;      (for [[i eav] eavs]
;        (do
;          (println i eav)
;          (swap! *session
;            (fn [session]
;              (-> session
;                (o/insert i eav)
;                (o/fire-rules))))))))
;  0)
;
;(add-movers)
;(add-painters)
;
;(defn q
;  []
;  (o/query-all @*session ::actor))
;
;
;