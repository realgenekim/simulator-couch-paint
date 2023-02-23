(ns genek.sim2
  (:require
    [clojure.spec.alpha :as s]
    [clojure.math.combinatorics :as combo]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [com.rpl.specter :as sp]
    [flow-storm.api]
    [genek.entities :as e]
    [genek.utils :as utils]
    [logging.main :as glog]
    [taoensso.timbre :as log]))

(glog/configure-logging! glog/config)

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



(defonce *state (atom [(create-state (e/create-rooms 4) (e/create-movers 2) (e/create-painters 4))]))

(def default-start-state
  (create-state (e/create-rooms 15) (e/create-movers 2) (e/create-painters 3)))

(defn init-state!
  []
  (reset! *state [default-start-state]))



(comment
  @*state
  (reset! *state [default-start-state])
  (init-state!)
  0)

(defn atom?
  [a]
  (instance? clojure.lang.Atom a))

(comment
  (atom? (atom nil))
  (atom? nil)
  0)

(s/def ::s-index nat-int?)
(s/def ::s-indexes
  (s/coll-of ::s-index))

; TODO: return full room, not id, so we can assoc in new value in advance state
(>defn workers-working
  " given state, return vector of all all workers workign
    [{:id :role :at-room}] "
  [kworkers state] [keyword? map? => ::e/s-movers]
  (->> state
    kworkers
    (filter :at-room)
    vec
    #_((fn [movers]
         (->> movers
           ; :at-room are all the room numbers that the movers are at
           (filter #(:at-room %)))))))

; TODO: move this to entities

(def workers-moving (partial workers-working :movers))
(def workers-painting (partial workers-working :painters))

(>defn room-has-painter
  " does the room have a painter there? "
  [state roomnum] [::e/s-state nat-int? => boolean?]
  (let [rooms (into #{} (->> (workers-painting state)
                          (map :at-room)))]
    (boolean (rooms roomnum))))

; ^^^^^ MOVE






(>defn next-turn!
  " updates state atom: append the new state to end of *states vector
    if no args, copy state and increment turn"
  ([*states] [atom? => sequential?]
   (swap! *states conj
     (let [state (last @*states)]
       (assoc state :turn (inc (:turn state))))))
  ([*states newstate] [atom? map? => sequential?]
   (swap! *states conj
     (assoc newstate :turn (inc (:turn newstate))))))

(>defn next-turn
  " for testing, just advance the turn
    "
  ([state opts] [::e/s-state map? => ::e/s-state]
   (assoc state :turn (inc (:turn state))))
  ([state] [::e/s-state => ::e/s-state]
   (next-turn state {})))

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




(comment
  (e/rooms-needing-movers (-> @*state last :rooms))
  0)


;
; painters
;

(s/def ::s-moving-assignment
  (s/keys :opt-un [::room ::mover]))
(s/def ::s-moving-assignments
  (s/coll-of ::s-moving-assignment))

(s/def ::assignments ::s-moving-assignment)
(s/def ::all-choices
  (s/keys :opt-un [::rooms-need-painters ::painters]))

(s/def ::s-moving-assignments-and-choices
  (s/keys :opt-un [::choice ::all-choices]))

(>defn- vecmap->room-assignments
  " input as tuplies of [room, worker]
       assign the room, which means we change room state, change mover :at-room
    input: kworker: :mover or :painter
           [[ room mover] ...] (created by map vector of rooms needing moving, and available movers)
    output: {:room ... :mover ...}"
  [kworker [room worker]] [keyword? vector? => ::s-moving-assignment]
  ; case 1: no movers
  ; case 2: more movers than rooms
  ; case 3: mover rooms than mover
  ;
  ; put them into one vector
  (log/debug :vecmap->room-assignments :room room)
  (log/debug :vecmap->room-assignments :worker (str worker))
  (if (and room worker)
    (let [roomstate (-> room :state)
          ; update room state, given that we've just assigned a worker
          newroom   (assoc room :state
                                (cond
                                  (and
                                    (= :waiting-for-movers1 roomstate)
                                    (= (worker :role) :mover))
                                  (do
                                    (log/warn "*** " :vecmap->room-assignments :set :removing-furniture)
                                    :removing-furniture)

                                  (and
                                    (= :waiting-for-painters roomstate)
                                    (= (worker :role) :painter))
                                  (do
                                    (log/warn "*** " :vecmap->room-assignments :set :painting)
                                    :painting)

                                  (and
                                    (= :waiting-for-movers2 roomstate)
                                    (= (worker :role) :mover))
                                  (do
                                    (log/warn "*** " :vecmap->room-assignments :set :restoring-furniture)
                                    :restoring-furniture)

                                  :else
                                  roomstate))

          newworker (assoc worker :at-room (-> room :id))
          retval    {:room   newroom
                     kworker newworker}]
      (log/warn :vecmap->room-assignments :retval retval)
      retval)))

(>defn- create-mover-assignments
  " for every room that needs mover/painter, identify a mover to be assigned
    input: state
    output: [{:room .. :mover} ...] "
  [state] [::e/s-state => ::s-moving-assignments]
  (let [needs-movers     (e/rooms-needing-movers (-> state :rooms))
        movers           (e/available-movers state)
        _                (log/debug :create-mover-assignments :needs-movers needs-movers)
        _                (log/debug :create-mover-assignments :movers movers)
        room+movers      (map vector needs-movers movers)
        ; this creates [{:room newroom :mover newmover}...]
        _                (log/debug :create-mover-assignments :rooms+movers room+movers)
        new-rooms+movers (->> room+movers
                           (map #(vecmap->room-assignments :mover %))
                           (remove nil?))]
    (log/debug :create-mover-assignments :new-room-movers
      (with-out-str (clojure.pprint/pprint new-rooms+movers)))
    new-rooms+movers))

(>defn- apply-moving-assignments
  " input:  state
            moving assignments: [{:room .. :mover} ...] : these are moving assigments, created by create-mover-assignments
    output: state "
  [state assignments] [::e/s-state ::s-moving-assignments => ::e/s-state]
  (log/debug :apply-moving-assignments :assignments assignments)
  (let [newstate (reduce
                   utils/update-rooms-movers
                   state [assignments])]
    #_(log/debug :apply-moving-assignments :new-room-movers
        (with-out-str (clojure.pprint/pprint new-rooms+movers)))
    newstate))

(>defn assign-movers
  " for every room that needs mover/painter, assign one that is available
  "
  ([state opts] [::e/s-state map? => ::e/s-state]
   (let [assignments (create-mover-assignments state)
         newstate    (apply-moving-assignments state assignments)]
     (log/debug :assign-movers :assignments (vec assignments))
     newstate))
  ([state] [::e/s-state => ::e/s-state]
   (assign-movers state {})))

(>defn free-movers
  " for every room that has done mover/painter:
      advance room state
      set mover :at-room to nil
  "
  ([state opts] [::e/s-state map? => ::e/s-state]
   (let [done-rooms  (->> (e/rooms-done-with-movers (-> state :rooms))
                          (map :id))
         ; ^^ list of rooms that are done (0 1 2)
         ; now we need to
         _           (log/debug :free-movers :done-rooms (vec done-rooms))
         newstate    (utils/free-room-movers state done-rooms)]
     newstate))
  ([state] [::e/s-state => ::e/s-state]
   (free-movers state {})))

;
; painters
;

(>defn- painter-potential-assignments
  " for every room that needs mover/painter, identify a mover to be assigned
    this requires the following steps:
       - find all rooms that need painters   /- these two, let's put into discover (the rest into choose assignment)
       - find all available painters        /
    input: state
    output:
             {:rooms-need-painters
              :painters} "
  ([state opts]
   ;[::e/s-state map? => ::s-moving-assignments-and-choices]
   [::e/s-state map? => map?]
   (let [needs-painters     (e/rooms-needing-painters state opts)
         painters           (e/available-painters state)
         _                  (log/warn :painter-potential-assignments :needs-movers (vec needs-painters))
         _                  (log/warn :painter-potential-assignments :painters (vec painters))]

     {:rooms-need-painters (vec needs-painters)
      :painters             (vec painters)}))
  ([state] [::e/s-state => map?]
   (painter-potential-assignments state {})))

(>defn- create-painter-assignments
  " for every room that needs mover/painter, identify a mover to be assigned
    this requires the following steps:
       - find all rooms that need painters    /- these two live in painter-potential-assignments
       - find all available painters         /
       - pick one <-- this is either O(1), or O(n!) (combinatorial, because we will search through all combinations of rooms to be assigned)
       - assign them to a room

    input:  state
    opts:   {:schedule {:rooms-needing-painting [{room } ...]}}
               ^^ when schedule is given, just use it

    output: [{:room .. :mover} ...]
    "
  ([state {:keys [painter-schedule strict] :as opts}]
   [::e/s-state map? => ::s-moving-assignments-and-choices]
   ;[::e/s-state map? => ::s-moving-assignments]
   (let [{:keys [rooms-need-painters painters]
          :as   all-choices} (painter-potential-assignments state opts)
         _                  (log/warn :create-painter-assignments :opt-painter-schedule painter-schedule :opts opts)

         ; was the order/selection/schedule of rooms given by parent?
         sched-provided      (-> opts :schedule :rooms-needing-painting)
         rooms-need-painters (if-not sched-provided
                               ; no-op
                               rooms-need-painters
                               ; else we were given it by parent
                               (do
                                 (log/warn :create-painter-assignments :schedule-provided!!! :old-sched (vec rooms-need-painters))
                                 (log/warn :create-painter-assignments :schedule-provided!!! (vec sched-provided))
                                 sched-provided))

         room+painters      (case (or painter-schedule :fifo)
                              ; this is what we need to lift up --
                              ;   a search would rotate/cycle (if all equal)
                              ;   or would find all combinations
                              ;       https://stackoverflow.com/questions/26076077/clojure-list-all-permutations-of-a-list
                              :fifo (map vector rooms-need-painters painters)
                              :lifo (map vector (reverse rooms-need-painters) painters)
                              :random (map vector (shuffle rooms-need-painters) painters))
         ; this creates [{:room newroom :mover newmover}...]
         _                  (log/debug :create-painter-assignments :rooms+painters room+painters)
         new-rooms+painters (->> room+painters
                              (map #(vecmap->room-assignments :painter %))
                              (remove nil?))]
     (log/debug :create-painter-assignments :new-room-movers
       (with-out-str (clojure.pprint/pprint new-rooms+painters)))
     {:choice new-rooms+painters
      :all-choices all-choices}))
  ([state] [::e/s-state => ::s-moving-assignments-and-choices]
   (create-painter-assignments state {})))


(>defn- apply-painting-assignments
  " input:  state
            moving assignments: [{:room .. :mover} ...] : these are moving assigments, created by create-mover-assignments
    output: state "
  [state assignments] [::e/s-state ::s-moving-assignments => ::e/s-state]
  (log/debug :apply-painting-assignments :assignments assignments)
  (let [newstate (reduce
                   utils/update-rooms-painters
                   state [assignments])]
    #_(log/debug :apply-painting-assignments :new-room-movers
        (with-out-str (clojure.pprint/pprint new-rooms+movers)))
    newstate))

(>defn assign-painters
  " for every room that needs mover/painter, assign one that is available
    input: state
  "
  ([state opts] [::e/s-state map? => ::e/s-state]
   (let [all-assignments (create-painter-assignments state opts)
         assignments     (-> all-assignments :choice)
         newstate        (-> (apply-painting-assignments state assignments)
                           (assoc-in [:metadata :painter-schedule-choices]
                             (-> all-assignments :all-choices)))]
     newstate))
  ([state] [::e/s-state => ::e/s-state]
   (assign-painters state {})))

(comment
  (e/rooms-done-with-movers (-> @*state last :rooms))
  (free-movers (-> @*state last :rooms))
  0)

(>defn free-painters
  " for every room that has done mover/painter:
      advance room state
      set mover :at-room to nil
  "
  ([state opts] [::e/s-state map? => ::e/s-state]
   (let [done-rooms (->> (e/rooms-done-with-painters (-> state :rooms))
                      (map :id))
         ; ^^ list of rooms that are done (0 1 2)
         ; now we need to
         _          (log/debug :free-painters :done-rooms done-rooms)
         newstate   (utils/free-room-painters state done-rooms)]
     newstate))
  ([state] [::e/s-state => ::e/s-state]
   (free-painters state {})))

(>defn rooms-needs-painter-already-there
  " there are now conditions where painters are already in room, and we haven't transitioned room states
    find those conditions:  room needs painting, and painter is there
    "
  [state] [::e/s-state => ::e/s-rooms]
  ; for each room
  ;  if state is :waiting-for-painters, and room has a painter there
  ;    then flip state to painting
  (let [retval (->> (-> state :rooms)
                 (mapv (fn [{:keys [id]
                             :as   room}]
                         (if (not= (-> room :state) :waiting-for-painters)
                           ; unchanged
                           room
                           ; otherwise flip state
                           (if (room-has-painter state id)
                             (assoc room :state :painting)
                             room)))))]
    (log/debug :rooms-needs-painter-already-there :retval retval)
    retval))

(>defn advance-state
  " IMPORTANT: take care of things like
    - decrementing working counters (e.g., :moving1-time-remaining) of all rooms with movers/painters assigned
    - change state of room (TODO: really true?)
    - unassigning movers and painters (XXX: isn't this done elsewhere?) "
  ([state opts] [::e/s-state map? => ::e/s-state]
   (let [rooms-moving   (workers-moving state)
         rooms-painting (workers-painting state)
         combined       (vec (flatten (conj rooms-moving rooms-painting)))]
     (log/debug :advance-state/entering :rooms-moving rooms-moving)
     (log/debug :advance-state/entering :rooms-painting rooms-painting)
     (log/debug :advance-state/entering :rooms-combined combined)
     (log/debug :advance-state/entering :state state)
     ; intentially shadow state var
     (reduce (fn [state rs]
               (log/debug :advance-state :reduce/entering :state :s (utils/pp-str-cr state))
               (log/debug :advance-state :reduce/entering :rooms-being-worked (utils/pp-str-cr rs))
               (if-not (empty? rs)
                 ; get the first worker, which is looks like: {:id 0, :role :mover, :at-room 0}
                 ; get the room number
                 ; decrement room counter based on current state
                 ;    :removing-furniture (dec :moving1-time-remaining)
                 ;    :painting  (dec :painting-time-remaining)
                 ;    :restoring-furniture (dec :moving2-time-remaining)
                 (let [
                       worktask  (first rs)
                       _         (log/debug :advance-state :reduce/entering :process worktask)
                       roomnum   (:at-room worktask)
                       oldroom   (utils/get-by-id (-> state :rooms) roomnum)
                       _         (log/debug :advance-state :old-room oldroom)
                       roomstate (:state oldroom)
                       newroom   (cond
                                   (and
                                     (= :removing-furniture roomstate)
                                     (= (worktask :role) :mover))
                                   (do
                                     (log/debug "*** " :advance-state :decrementing :moving1-time-remaining)
                                     (-> oldroom
                                       (update-in [:moving1-time-remaining] dec)
                                       (update-in [:furniture-stored] (fnil inc 0))))

                                   (and
                                     (= :painting roomstate)
                                     (= (worktask :role) :painter))
                                   (do
                                     (log/debug :advance-state :decrementing :painting-time-remaining)
                                     (update-in oldroom [:painting-time-remaining] dec))

                                   (and
                                     (= :restoring-furniture roomstate)
                                     (= (worktask :role) :mover))
                                   (do
                                     (log/debug :advance-state :decrementing :moving2-time-remaining)
                                     (-> oldroom
                                       (update-in [:moving2-time-remaining] dec)
                                       (update-in [:furniture-stored] (fnil dec 0))))

                                   :else
                                   nil)
                       ; handle case of painters already there, and needs to start painting
                       newrooms  (if newroom
                                   (utils/update-by-id (-> state :rooms) newroom)
                                   ; leave unchanged
                                   (-> state :rooms))
                       ;newrooms  (rooms-needs-painter-already-there
                       ;            (assoc state :rooms newrooms))
                       new-state (-> state
                                   (assoc :rooms newrooms)
                                   (assoc-in [:furniture :in-storage]
                                     (->> newrooms
                                       (map :furniture-stored)
                                       (remove nil?)
                                       (reduce +)))
                                   (assoc-in [:furniture :max-in-storage]
                                     (max
                                       (or (get-in state [:furniture :in-storage]) 0)
                                       (or (get-in state [:furniture :max-in-storage]) 0))))]

                   (recur new-state (rest rs)))
                 ; termination case
                 state))
       ; change states to :painting if painter is there
       (assoc state :rooms
                    (rooms-needs-painter-already-there state))
       [combined])))
  ([state] [::e/s-state => ::e/s-state]
   (advance-state state {})))


(comment
  (->
    (last @*state)
    advance-state)
  (->> [{:a 1} {:a 2} {:b 3}]
    (map :a)
    (remove nil?)
    (reduce +))
  0)


(>defn simulate-turn
  "execute all steps in a turn: takes a state, returns a state"
  ([state opts] [::e/s-state map? => ::e/s-state]
   (-> state
     (assign-movers opts)
     (free-movers opts)
     (assign-painters opts)
     (free-painters opts)
     (advance-state opts)
     (next-turn opts)))
  ([state] [::e/s-state => ::e/s-state]
   (simulate-turn state {})))

; main interface
(>defn simulate-until-done
  " this is responsible for running the sim
    input: initial state
    output: sequence of states, run through state machine"
  ; 3 arity, build upon state
  ([state states {:keys [maxturns update-state-atom?]
                  :or {update-state-atom? true}
                  :as opts}] [::e/s-state ::e/s-states map? => ::e/s-states]
   ; save to global var so we can watch
   (log/warn :simulate-until-done :opts opts)
   (if update-state-atom?
     (reset! *state states))
   (let [newstate (simulate-turn state opts)]
     ; if done return, else recurse
     (log/warn :simulate-until-done :turn (-> newstate :turn))
     (if (or
           (e/all-rooms-finished? state)
           (and maxturns
             (> (-> newstate :turn) maxturns)))
           ; to limit run
           ;(> (-> newstate :turn) 2000))
       states

       ; else
       (recur newstate (conj states newstate) opts))))
  ; 2 arity: create new states
  ([state opts] [::e/s-state map? => ::e/s-states]
   (simulate-until-done state [state] opts))
  ([state] [::e/s-state => ::e/s-states]
   (simulate-until-done state {})))


; find min

(>defn simulate-with-painting-choice
  " this is responsible for running the sim
    input: initial state
    output: sequence of states, run through state machine"
  ; 3 arity, build upon state
  ([state states {:keys [maxturns]
                  :as opts}] [::e/s-state ::e/s-states map? => ::e/s-states]
   ; save to global var so we can watch
   (log/warn :simulate-with-painting-choice :opts opts)
   (reset! *state states)
   (let [newstate (simulate-turn state opts)]
     ; if done return, else recurse
     (log/debug :simulate-with-painting-choice :turn (-> newstate :turn))
     (if (or
           (e/all-rooms-finished? state)
           (and maxturns
             (> (-> state :turn) maxturns)))
       ;(> (-> state :turn) 200))
       states

       ; else
       (recur newstate (conj states newstate) opts)))))
  ; 2 arity: create new states
  ;([state opts] [::e/s-state map? => ::e/s-states]
  ; (simulate-until-done state [state] opts))
  ;([state] [::e/s-state => ::e/s-states]
  ; (simulate-until-done state {})))

(>defn simulate-find-min
  " this is responsible for running the sim
    input: initial state
    output: sequence of states, run through state machine"
  ; 3 arity, build upon state
  ([state states {:keys [maxturns]
                  :as opts}] [::e/s-state ::e/s-states map? => ::e/s-states]
   ; save to global var so we can watch
   (log/warn :simulate-find-min :opts opts)
   (reset! *state states)

   (log/error :simulate-find-min "****************************")
   (log/error :simulate-find-min :turn (-> state :turn))
   ; done?
   (if (or
         (> (-> state :turn) 200)
         (e/all-rooms-finished? state)
         (and maxturns
           (> (-> state :turn) maxturns)))
     states
     ; else: algorithm
     ;   - do one run, to get all the painter choices
     ;   - then iterate to find the minimum
     (let [setup-run                (simulate-turn state opts)
           _                        (log/warn :simulate-find-min :state setup-run)
           painter-schedule-choices (-> setup-run :metadata :painter-schedule-choices)
           _ (log/warn :simulate-find-min :painter-schedule-choices painter-schedule-choices)
           {:keys [rooms-need-painters painters]} painter-schedule-choices
           room-permutations        (combo/permutations
                                      (-> setup-run :metadata :painter-schedule-choices :rooms-need-painters))]

       (log/warn :simulate-find-min :count (count room-permutations) :permutations (vec room-permutations))
       ; call next-turn here on all permutations, and then pick the one with the least cost
       (let [runs (for [rp (->> room-permutations (take 1))]
                    (simulate-turn state
                      (merge opts {:schedule {:rooms-needing-painting rp}
                                   :update-state-atom? false})))
             min-run (->> runs
                       (sort-by #(fn [r]
                                   (count r)))
                       first)]

         (def runs runs)
         (doseq [r runs]
           (log/warn :simulate-find-min :print-out-each-score (count r)))
         ;all-choices
         (recur min-run (conj states min-run) opts))))
   #_(let [newstate (simulate-turn state opts)]
       ; if done return, else recurse
       (log/debug :simulate-find-min :turn (-> newstate :turn))
       (if (or
             (e/all-rooms-finished? state)
             (and maxturns
               (> (-> state :turn) maxturns)))
         ;(> (-> state :turn) 200))
         states

         ; else
         (recur newstate (conj states newstate) opts))))
  ; 2 arity: create new states
  ([state opts] [::e/s-state map? => ::e/s-states]
   (simulate-find-min state [state] opts))
  ([state] [::e/s-state => ::e/s-states]
   (simulate-find-min state {})))



(comment
  (-> runs count)
  (-> runs count)
  (for [r runs]
    (count r))


  (tap> runs)
  (-> runs first count)
  (-> *state deref count)
  (tap> (-> *state deref))
  (do
    (reset! *state (first runs))
    nil)

  (init-state!)
  (reset! *state [default-start-state])

  (->> @*state
    (take 50)
    (map e/all-rooms-finished?))

  (-> @*state (nth 68)
    (e/all-rooms-finished?))


  (let [state {:turn     0,
               :rooms    [{:id                      0,
                           :role                    :room,
                           :state                   :waiting-for-painters,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 10,
                           :moving2-time-remaining  10}
                          {:id                      1,
                           :role                    :room,
                           :state                   :waiting-for-painters,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 10,
                           :moving2-time-remaining  10}
                          {:id                      2,
                           :role                    :room,
                           :state                   :waiting-for-painters,
                           :moving1-time-remaining 0,
                           :painting-time-remaining 10,
                           :moving2-time-remaining  10}]
               :movers   [{:id 0, :role :mover, :at-room nil}],
               :painters [{:id 0, :role :painter, :at-room nil}
                          {:id 1, :role :painter, :at-room nil}]
               :furniture-stored 0}]

    (let [newstates (simulate-find-min state {:strict true})]
      (def newstates newstates)))
      ; two turns?
      ;(is (= [0 1]
      ;      (->> newstates (mapv :turn))))))


  (init-state!)
  (do
    (reset! *state runs)
    nil)
  (count newstates)

  0)

