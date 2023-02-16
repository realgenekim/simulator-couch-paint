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
    input: m:       {:old-rooms ... :old-movers ...} (old state)
           new-rms: [{:mover .. :room ..} {}] (set of tuples, mover -> new room)
    output: {:old-rooms :old-movers}

    XXX: signature should be state, move-assingments"
  [{:keys [old-rooms old-movers] :as m} new-rms]
  [map? (s/nilable sequential?) => map?]
  ; ending case
  (println :update-rooms-movers :m (pp-str m)
    :new-rms (pp-str new-rms))
  ; empty or nil
  (if (empty? new-rms)
    {:old-rooms old-rooms
     :old-movers old-movers}
    ; else
    (let [newrooms (update-by-id old-rooms (:room (first new-rms)))
          newmovers (update-by-id old-movers (:mover (first new-rms)))]
      (recur
        {:old-rooms newrooms
         :old-movers newmovers}
        (rest new-rms)))))

(>defn update-rooms-movers2
  " reducing function
    input: state
           room-assignments: [{:mover .. :room ..} {}] (set of tuples, mover -> new room)
    output: new state "
  [{:keys [rooms movers] :as state} room-assignments]
  [map? (s/nilable sequential?) => map?]
  ; ending case
  (println :update-rooms-movers2 :m (pp-str state) :new-rms (pp-str room-assignments))
  ; empty or nil
  (if (empty? room-assignments)
    state
    (let [newrooms  (update-by-id rooms (:room (first room-assignments)))
          newmovers (update-by-id movers (:mover (first room-assignments)))]
      (recur (assoc state :rooms newrooms :movers newmovers)
        (rest room-assignments)))))

(>defn free-room-movers
  " reducing function
    input: {:old-rooms ... :old-movers ... :new-rms [{:mover .. :room ..} {}]
    output: same "
  [{:keys [old-rooms old-movers] :as m} new-rms]
  [map? (s/nilable sequential?) => map?]
  ; ending case
  (println :update-rooms-movers :m (pp-str m)
    :new-rms (pp-str new-rms))
  ; empty or nil
  (if (empty? new-rms)
    {:old-rooms old-rooms
     :old-movers old-movers}
    ; else
    (let [newrooms (update-by-id old-rooms (:room (first new-rms)))
          newmovers (update-by-id old-movers (:mover (first new-rms)))]
      (recur
        {:old-rooms newrooms
         :old-movers newmovers}
        (rest new-rms)))))