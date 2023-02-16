(ns genek.utils
  (:require
    [clojure.spec.alpha :as s]
    [com.rpl.specter :as sp]
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

(>defn update-by-id
  " input: sequence of maps (rooms, movers, painters), and new record with {:id } to replace record
    output: seq of maps, with replace record "
  [ms newmap] [::e/s-records map? => ::e/s-records]
  (->> ms
    (map (fn [m]
           (if (= (:id m) (:id newmap))
             newmap
             m)))))

(>defn update-by-id-apply-fn
  " input: sequence of maps (rooms, movers, painters), and new record with {:id } to apply f
    output: seq of maps, with replace record "
  [ms id f] [::e/s-records integer? fn? => ::e/s-records]
  (->> ms
    (map (fn [m]
           (if (= (:id m) id)
             (-> m f)
             m)))))

(comment
  ; all these are equivalent
  (update-by-id-apply-fn
    [{:id 1 :n 1} {:id 2 :n 3}]
    1
    #(update-in % [:n] inc))

  (sp/transform [0 :n] inc [{:id 1 :n 1} {:id 2 :n 3}])
  (sp/transform [sp/ALL (sp/pred #(= 1 (:id %))) :n] inc [{:id 1 :n 1} {:id 2 :n 3}])

  0)

;
; update
;

(>defn update-rooms-movers
  " reducing function
    input: state
           room-assignments: [{:mover .. :room ..} {}] (set of tuples, mover -> new room)
    output: new state "
  [{:keys [rooms movers] :as state} room-assignments]
  [map? (s/nilable sequential?) => map?]
  (println :update-rooms-movers :m (pp-str state) :new-rms (pp-str room-assignments))
  ; empty or nil
  (if (empty? room-assignments)
    state
    (let [newrooms  (update-by-id rooms (:room (first room-assignments)))
          newmovers (update-by-id movers (:mover (first room-assignments)))]
      (recur (assoc state :rooms newrooms :movers newmovers)
        (rest room-assignments)))))

(>defn free-room-movers
  " reducing function
      for every room that has done mover/painter:
        advance room state
        set mover :at-room to nil
    input: state
           done-rooms: vector of room numbers: [0 1 2]
    output: new state "
  [{:keys [rooms movers] :as state} done-rooms]
  [map? (s/nilable sequential?) => map?]
  (println :free-room-movers :m (pp-str state) :new-rms (pp-str done-rooms))
  ; empty or nil
  (if (empty? done-rooms)
    state
    (let [roomnum   (first done-rooms)]
          ;newrooms  (update-by-id rooms (:room (first done-rooms)))
          ;newmovers (update-by-id movers (:mover (first done-rooms)))]
      ;(sp/transform [sp/ALL (sp/pred #(= 1 (:id %))) :n] inc [{:id 1 :n 1} {:id 2 :n 3}])
      (->> state
        ; room: advance to next state
        ((fn [x]
           (let [rstate (-> (sp/select [:rooms roomnum :state] x) last)]
             ;rstate))))
             (sp/setval [:rooms roomnum :state] (get e/next-room-state rstate) x))))
        ; mover: set :at-room to nil
        (sp/setval [:movers sp/ALL (sp/pred #(= roomnum (:at-room %))) :at-room] nil)))
    #_(recur (assoc state :rooms newrooms :movers newmovers)
        (rest done-rooms))))


(comment
  (def roomnum 0)
  (->> {:turn 0,
        :rooms [{:id 0,
                 :role :room,
                 :state :removing-furniture
                 :moving1-time-remaining 10,
                 :painting-time-remaining 50,
                 :moving2-time-remaining 10}
                {:id 1,
                 :role :room,
                 :state :waiting-for-movers1,
                 :moving1-time-remaining 10,
                 :painting-time-remaining 50,
                 :moving2-time-remaining 10}]
        :movers [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room 0}],
        :painters [{:id 0, :role :painter, :at-room nil}
                   {:id 1, :role :painter, :at-room nil}
                   {:id 2, :role :painter, :at-room nil}
                   {:id 3, :role :painter, :at-room nil}]}
    (sp/setval [:movers sp/ALL (sp/pred #(= roomnum (:at-room %))) :at-room] nil))
  0)