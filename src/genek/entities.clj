(ns genek.entities
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]))


; mover or painter record
(s/def ::s-record
  (s/keys :req-un [::id]))
(s/def ::s-records
  (s/coll-of ::s-record))


(def room-states
  [:initial
   :waiting-for-movers1
   :removing-furniture
   :waiting-for-painters
   :painting
   :waiting-for-movers2
   :restoring-furniture
   :finished])

(def state-needs-mover?
  {:waiting-for-movers1 true
   :waiting-for-movers2 true})

(def state-needs-painter?
  {:waiting-for-painters true})

(def MOVING1-OP-TURNS 10)
(def PAINTING-OP-TURNS 50)
(def MOVING2-OP-TURNS 10)


(>defn create-room
  " input: id of room"
  [id] [integer? => map?]
  {:id id
   :role :room
   ; start with :waiting-for-movers1
   :state (second room-states)
   :moving1-time-remaining MOVING1-OP-TURNS
   :painting-time-remaining PAINTING-OP-TURNS
   :moving2-time-remaining MOVING2-OP-TURNS})

(>defn create-rooms
  "input: # of rooms"
  [n] [integer? => sequential?]
  (for [r (range n)]
    (create-room r)))

(>defn create-mover
  [id] [integer? => map?]
  {:id id
   :role :mover
   :at-room nil})

(>defn create-movers
  "input: # of movers"
  [n] [integer? => sequential?]
  (for [r (range n)]
    (create-mover r)))

(>defn create-painter
  [id] [integer? => map?]
  {:id id
   :role :painter
   :at-room nil})

(>defn create-painters
  "input: # of painters"
  [n] [integer? => sequential?]
  (for [r (range n)]
    (create-painter r)))
