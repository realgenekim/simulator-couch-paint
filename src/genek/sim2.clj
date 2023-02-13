(ns genek.sim2
  (:require
    [clojure.spec.alpha :as s]
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

(def state-needs-mover?
  {:waiting-for-movers1 true
   :waiting-for-movers2 true})

(def state-needs-painter?
  {:waiting-for-painters true})

(def MOVING1-OP-TURNS 10)
(def PAINTING-OP-TURNS 50)
(def MOVING2-OP-TURNS 10)


(>defn create-room
  [id] [integer? => map?]
  {:id id
   :role :room
   ; start with :waiting-for-movers1
   :state (second room-states)
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
  " updates state atom "
  [*states] [atom? => sequential?]
  (swap! *states conj
    (let [state (last @*states)]
      (assoc state :turn (inc (:turn state))))))

(>defn increment-state
  " identity, but just increment turn "
  [state] [map? => map?]
  (assoc state :turn (inc (:turn state))))

(>defn next-turn-fn!
  " updates state atom, using f to update turn "
  [*states f] [atom? fn? => sequential?]
  (swap! *states conj
    (let [state (last @*states)]
      (f state))))

(comment
  (next-turn! *state)
  (swap! *state next-turn)
  0)

(s/def ::s-room
  (s/keys :req-un [::role ::state ::id ::painting-time-remaining
                   ::moving1-time-remaining ::moving2-time-remaining]))
(s/def ::s-rooms
  (s/coll-of ::s-room))

(s/def ::s-mover
  (s/keys :req-un [::id ::role ::at-room]))
(s/def ::s-movers
  (s/coll-of ::s-mover))

(>defn rooms-needing-movers
  " input: all rooms
    output: all rooms that need movers"
  [rooms] [::s-rooms => ::s-rooms]
  (->> rooms
       (filter (fn [r]
                 (let [state (-> r :state)]
                   (get state-needs-mover? state))))))

(comment
  (rooms-needing-movers (-> @*state last :rooms))
  0)

(>defn available-movers
  " input: all movers
    output: all movers that are available "
  [movers] [::s-movers => ::s-movers]
  (->> movers
    (filter (fn [m]
              (nil? (:at-room m))))))


(comment
  (available-movers (-> @*state last :movers))
  0)


(>defn nt-assign-avail-resources
  " for every room that needs mover/painter, assign one that is available
  "
  [state] [map? => map?])


