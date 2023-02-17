(ns genek.utils
  (:require
    [clojure.spec.alpha :as s]
    [com.rpl.specter :as sp]
    [flow-storm.api]
    [genek.entities :as e]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]))

(defn pp-str
  [x]
  (with-out-str
    (clojure.pprint/pprint x)))

; https://stackoverflow.com/questions/40370240/easy-way-to-change-specific-list-item-in-list

(defn list-update-in [l i x]
  " input: list l, index, item "
  (let [newlist (take i l)
        newlist (concat newlist (list x))
        newlist (concat newlist (drop (+ 1 i) l))]
    newlist))

(>defn get-by-id
  " input: sequence of maps (rooms, movers, painters), and new record with {:id } to replace record
    output: seq of maps, with replace record "
  [ms id] [::e/s-records nat-int? => ::e/s-record]
  (get ms id))

(>defn update-by-id
  " input: sequence of maps (rooms, movers, painters), and new record with {:id } to replace record
    output: seq of maps, with replace record "
  [ms newmap] [::e/s-records map? => ::e/s-records]
  (->> ms
    (map (fn [m]
           (if (= (:id m) (:id newmap))
             newmap
             m)))
    vec))

(>defn update-by-id-apply-fn
  " input: sequence of maps (rooms, movers, painters), and new record with {:id } to apply f
    output: seq of maps, with replace record "
  [ms id f] [::e/s-records integer? fn? => ::e/s-records]
  {:pre  [(nat-int? id)]}
  (->> ms
    (map (fn [m]
           (if (= (:id m) id)
             (-> m f)
             m)))
    vec))

(comment
  ; all these are equivalent
  (update-by-id-apply-fn
    [{:id 1 :n 1} {:id 2 :n 3}]
    1
    #(update-in % [:n] inc))

  (sp/transform [0 :n] inc (list {:id 1 :n 1} {:id 2 :n 3}))
  ; ^^ omg, doesn't work!
  (sp/transform [sp/ALL (sp/pred #(= 1 (:id %))) :n] inc [{:id 1 :n 1} {:id 2 :n 3}])

  0)

;
; update
;


(>defn- update-rooms-workers
  " reducing function
    input: state
           room-assignments: [{:mover .. :room ..} {}] (set of tuples, mover -> new room)
    output: new state "
  [kworker {:keys [rooms] :as state} room-assignments]
  [keyword? map? (s/nilable sequential?) => map?]
  (println :update-rooms-workers :worker kworker :state (pp-str state) :room-assignments (pp-str room-assignments))
  ; empty or nil
  (if (empty? room-assignments)
    state
    (let [newrooms   (update-by-id rooms (:room (first room-assignments)))
          workers    (case kworker
                       :movers (-> state :movers)
                       :painters (-> state :painters))
          _          (println :update-rooms-workers :assignments (kworker (first room-assignments)) :workers workers)
          newworkers (update-by-id workers
                       ; get :mover or :painter key in assignment
                       ((case kworker
                          :movers :mover
                          :painters :painter) (first room-assignments)))]
      (recur kworker (assoc state :rooms newrooms kworker newworkers)
        (rest room-assignments)))))

(def update-rooms-movers (partial update-rooms-workers :movers))
(def update-rooms-painters (partial update-rooms-workers :painters))

(>defn free-room-movers
  " reducing function
      for every room that has done mover/painter:
        advance room state (:removing-furniture -> :waiting-for-painters)
        set mover :at-room to nil
    input: state
           done-rooms: vector of room numbers: [0 1 2]
    output: new state "
  [state done-rooms]
  [::e/s-state sequential? => ::e/s-state]
  {:pre [(vector? (-> state :rooms))
         (vector? (-> state :movers))
         (vector? (-> state :painters))]}
  (println :free-room-movers :state (pp-str state) :done-rooms (pp-str done-rooms))
  (loop [state state
         done-rooms done-rooms]
    ;(tap> "done-rooms")
    ;(tap> drooms)
    ; empty or nil
    (if (empty? done-rooms)
      (do
        (println :free-room-movers :done)
        state)
      (let [roomnum (first done-rooms)
            _       (println :free-room-movers :roomnum roomnum)
            newstate (->> state
                       ; room: advance to next state
                       ((fn [x]
                          ;(tap> "state")
                          ;(tap> curr-state)
                          ;(tap> "roomnum")
                          (let [rstate (-> (sp/select [:rooms roomnum :state] x)
                                         first)
                                nextstate (get e/next-room-state rstate)]
                            ;(tap> "nextstate")
                            (println :free-room-movers :setting :roomnum roomnum :rstate rstate :nextstate nextstate)
                            ;rstate))))
                            (sp/setval [:rooms roomnum :state] nextstate x))))
                       ; mover: set :at-room to nil
                       (sp/setval [:movers sp/ALL (sp/pred #(= roomnum (:at-room %))) :at-room] nil))]
        (recur newstate (rest done-rooms))))))


(comment

  ; 14m!
  (loop [n     1
         state {:turn     10,
                :rooms    '({:id                      0,
                             :role                    :room,
                             :state                   :removing-furniture,
                             :moving1-time-remaining  0,
                             :painting-time-remaining 50,
                             :moving2-time-remaining  10}
                            {:id                      1,
                             :role                    :room,
                             :state                   :removing-furniture,
                             :moving1-time-remaining  1,
                             :painting-time-remaining 50,
                             :moving2-time-remaining  10}
                            {:id                      2,
                             :role                    :room,
                             :state                   :waiting-for-movers1,
                             :moving1-time-remaining  10,
                             :painting-time-remaining 50,
                             :moving2-time-remaining  10}
                            {:id                      3,
                             :role                    :room,
                             :state                   :waiting-for-movers1,
                             :moving1-time-remaining  10,
                             :painting-time-remaining 50,
                             :moving2-time-remaining  10}),
                :movers   '({:id 0, :role :mover, :at-room 0} {:id 1, :role :mover, :at-room 1}),
                :painters '({:id 0, :role :painter, :at-room nil}
                            {:id 1, :role :painter, :at-room nil}
                            {:id 2, :role :painter, :at-room nil}
                            {:id 3, :role :painter, :at-room nil})}]
    (let [newstate (-> state
                     genek.sim2/assign-movers
                     genek.sim2/free-movers
                     genek.sim2/advance-state)]
      (def newstate newstate)
      (println "done")))

  (free-room-movers nil [0])

  (def roomnumg 0)
  (->> {:turn     10,
        :rooms    '({:id                      0,
                     :role                    :room,
                     :state                   :removing-furniture,
                     :moving1-time-remaining  0,
                     :painting-time-remaining 50,
                     :moving2-time-remaining  10}
                    {:id                      1,
                     :role                    :room,
                     :state                   :removing-furniture,
                     :moving1-time-remaining  1,
                     :painting-time-remaining 50,
                     :moving2-time-remaining  10}
                    {:id                      2,
                     :role                    :room,
                     :state                   :waiting-for-movers1,
                     :moving1-time-remaining  10,
                     :painting-time-remaining 50,
                     :moving2-time-remaining  10}
                    {:id                      3,
                     :role                    :room,
                     :state                   :waiting-for-movers1,
                     :moving1-time-remaining  10,
                     :painting-time-remaining 50,
                     :moving2-time-remaining  10}),
        :movers   '({:id 0, :role :mover, :at-room 0} {:id 1, :role :mover, :at-room 1}),
        :painters '({:id 0, :role :painter, :at-room nil}
                    {:id 1, :role :painter, :at-room nil}
                    {:id 2, :role :painter, :at-room nil}
                    {:id 3, :role :painter, :at-room nil})}
    ((fn [x]
       (let [rstate    (-> (sp/select [:rooms roomnumg :state] x) first)
             nextstate (get e/next-room-state rstate)]
         (println :free-room-movers :setting :roomnum roomnumg :rstate rstate :nextstate nextstate)
         rstate))))
         ;(sp/setval [:rooms roomnum :state] nextstate x)))))
    ;(sp/setval [:movers sp/ALL (sp/pred #(= roomnum (:at-room %))) :at-room] nil))
  0)