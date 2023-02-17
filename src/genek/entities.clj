(ns genek.entities
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]))


; mover or painter record

(s/def ::s-record
  (s/keys :req-un [::id]))
(s/def ::s-records
  (s/coll-of ::s-record))

(s/def ::state keyword?)

(s/def ::s-room
  (s/keys :req-un [::role ::state ::id ::painting-time-remaining
                   ::moving1-time-remaining ::moving2-time-remaining]))
(s/def ::s-rooms
  (s/coll-of ::s-room))

(s/def ::s-mover
  (s/keys :req-un [::id ::role ::at-room]))

(s/def ::s-movers
  (s/and
    (s/coll-of ::s-mover)
    vector?))

(s/def ::rooms ::s-rooms)
(s/def ::movers ::s-movers)

(s/def ::s-state
  (s/keys :req-un [::turn ::rooms ::movers ::painters]))

(def room-states
  [:initial
   :waiting-for-movers1
   :removing-furniture
   :waiting-for-painters
   :painting
   :waiting-for-movers2
   :restoring-furniture
   :finished])

(def next-room-state
  {:removing-furniture  :waiting-for-painters
   :painting            :waiting-for-movers2
   :restoring-furniture :finished})


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
  (-> (for [r (range n)]
        (create-mover r))
    vec))


(>defn create-painter
  [id] [integer? => map?]
  {:id id
   :role :painter
   :at-room nil})

(>defn create-painters
  "input: # of painters"
  [n] [integer? => sequential?]
  (-> (for [r (range n)]
        (create-painter r))
    vec))

;
;



;
; find
;

(>defn rooms-needing-movers
  " input: all rooms
    output: all rooms that need movers"
  [rooms] [::s-rooms => ::s-rooms]
  (->> rooms
    (filter (fn [r]
              (let [state (-> r :state)]
                (get state-needs-mover? state))))))

(>defn rooms-needing-painters
  " input: all rooms
    output: all rooms that need movers"
  [rooms] [::s-rooms => ::s-rooms]
  (->> rooms
    (filter (fn [r]
              (let [state (-> r :state)]
                (get state-needs-painter? state))))))

(>defn rooms-done-with-movers
  " input: all rooms
    output: all rooms that need movers"
  [rooms] [::s-rooms => ::s-rooms]
  (->> rooms
    (filter (fn [r]
              (or
                (and
                  (= :removing-furniture (-> r :state))
                  (zero? (-> r :moving1-time-remaining)))
                (and
                  (= :restoring-furniture (-> r :state))
                  (zero? (-> r :moving2-time-remaining))))))))

(>defn rooms-done-with-painters
  " input: all rooms
    output: all rooms that need movers"
  [rooms] [::s-rooms => ::s-rooms]
  (->> rooms
    (filter (fn [r]
              (and
                (= :painting (-> r :state))
                (zero? (-> r :painting-time-remaining)))))))

(>defn available-movers
  " input: state
    output: all movers that are available "
  [state] [::s-state => sequential?]
  (let [movers (-> state :movers)]
    (->> movers
      (filter (fn [m]
                (nil? (:at-room m)))))))

(>defn available-painters
  " input: state
    output: all movers that are available "
  [state] [::s-state => sequential?]
  (let [painters (-> state :painters)]
    (->> painters
      (filter (fn [m]
                (nil? (:at-room m)))))))


(comment
  (available-movers (-> @genek.simw/*state last :movers))
  0)


