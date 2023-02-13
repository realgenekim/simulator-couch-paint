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
   :role :room
   :state (first room-states)
   :moving1-time-remaining MOVING1-OP-TURNS
   :painting-time-remaining PAINTING-OP-TURNS
   :moving2-time-remaining MOVING2-OP-TURNS})

(def rooms (for [r (range 4)]
             (create-room r)))

(>defn create-mover
  [id] [integer? => map?]
  {:id id
   :role :mover
   :at-room nil})

(def movers (for [r (range 1)]
              (create-mover r)))

(>defn create-painter
  [id] [integer? => map?]
  {:id id
   :role :painter
   :at-room nil})

(def painters (for [r (range 1)]
                (create-painter r)))

(>defn create-state
  ([rooms movers painters]
   [sequential? sequential? sequential? => map?]
   {:turn     0
    :rooms    rooms
    :movers   movers
    :painters painters})
  ([turn rooms movers painters]
   [map? sequential? sequential? sequential? => map?]
   {:turn     (inc (:turn turn))
    :rooms    rooms
    :movers   movers
    :painters painters}))



(def *state (atom [(create-state rooms movers painters)]))

(comment
  @*state
  0)

(defn atom?
  [a]
  (instance? clojure.lang.Atom a))

(comment
  (atom? (atom nil))
  (atom? nil)
  0)

(>defn next-turn!
  [*states] [atom? => map?]
  (swap! *states conj
    (let [state (last @*states)]
      (assoc state :turn (inc (:turn state))))))

(comment
  (next-turn! *state)
  (swap! *state next-turn)
  0)