(ns genek.sim2-test
  (:require
    [clojure.test :refer :all]
    [com.rpl.specter :as sp]
    [genek.entities :as e]
    [genek.sim2 :as sim]
    [genek.utils :as utils]
    [taoensso.timbre :as log]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest primitives
  (testing "inc turn: base case"
    (let [newturn (sim/create-state [] [] [])]
      (is (= 0 (:turn newturn)))))
  (testing "inc turn: base case"
    (let [newturn (sim/create-state {:turn 1} [] [] [])]
      (is (= 2 (:turn newturn))))))

(deftest states
  (def *state (atom [(sim/create-state (e/create-rooms 4) (e/create-movers 1) (e/create-painters 1))]))
  (testing "next-turn with identity"
    (is (= 1 (count @*state)))

    (sim/next-turn! *state)
    (is (= 2 (count @*state))))

  (testing "next-turn-fn!"
    (sim/next-turn-fn! *state sim/increment-state)
    (is (= 3 (count @*state)))
    (is (= 2 (->> @*state last :turn)))

    (sim/next-turn-fn! *state sim/increment-state)
    (is (= 4 (count @*state)))
    (is (= 3 (->> @*state last :turn))))
  0)

(deftest update-records
  (testing "update-by-id"
    (let [ms     [{:id 1 :val 123}
                  {:id 2 :val 456}]
          newms  (utils/update-by-id ms {:id 1 :val 999})
          newms2 (utils/update-by-id ms {:id 2 :val 997})
          newms3 (utils/update-by-id ms {:id 3 :val 997})]
      (is (= 999
            (-> newms first :val)))
      (is (= 997
            (-> newms2 second :val)))
      (is (= ms newms3)))
    0))

(deftest match-resource
  (def *state (atom [(sim/create-state (e/create-rooms 4) (e/create-movers 1) (e/create-painters 1))]))

  (testing "needs movers/painters"
    (is (not= true
          (get e/state-needs-mover? :initial)))
    (is (= true
          (get e/state-needs-mover? :waiting-for-movers1)))
    (is (= true
          (get e/state-needs-mover? :waiting-for-movers2))))

  (testing "room needs movers"
    (let [needs-movers (e/rooms-needing-movers (-> @*state last :rooms))]
      (is (= 4
            (count needs-movers)))))

  (testing "next-turn-fn!"
    (sim/next-turn-fn! *state sim/increment-state)
    (is (= 2 (count @*state))))

  (testing "available movers"
    (let [available-movers (e/available-movers (-> @*state last))]
      (is (= 1
            (count available-movers))))))


(deftest moving
  (testing "isolate failing case"
    (let [state  {:turn     0,
                  :rooms    [{:id                      0,
                              :role                    :room,
                              :state                   :removing-furniture
                              :moving1-time-remaining  10,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  10}
                             {:id                      1,
                              :role                    :room,
                              :state                   :waiting-for-movers1,
                              :moving1-time-remaining  10,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  10}]
                  :movers   [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room 0}],
                  :painters [{:id 0, :role :painter, :at-room nil}
                             {:id 1, :role :painter, :at-room nil}
                             {:id 2, :role :painter, :at-room nil}
                             {:id 3, :role :painter, :at-room nil}]}
          retval (e/rooms-needing-movers (-> state :rooms))]
      (is (= [1]
            (->> retval
              (mapv :id)))))

    (testing "failing test case"
      (let [state {:turn 11,
                   :rooms
                   [{:id                      0,
                     :role                    :room,
                     :state                   :waiting-for-painters,
                     :moving1-time-remaining  0,
                     :painting-time-remaining 50,
                     :moving2-time-remaining  10}
                    {:id                      1,
                     :role                    :room,
                     :state                   :removing-furniture,
                     :moving1-time-remaining  0,
                     :painting-time-remaining 50,
                     :moving2-time-remaining  10}
                    {:id                      2,
                     :role                    :room,
                     :state                   :removing-furniture,
                     :moving1-time-remaining  10,
                     :painting-time-remaining 50,
                     :moving2-time-remaining  10}
                    {:id                      3,
                     :role                    :room,
                     :state                   :waiting-for-movers1,
                     :moving1-time-remaining  10,
                     :painting-time-remaining 50,
                     :moving2-time-remaining  10}],
                   :movers
                   [{:id 0, :role :mover, :at-room 2} {:id 1, :role :mover, :at-room 1}],
                   :painters
                   [{:id 0, :role :painter, :at-room nil}
                    {:id 1, :role :painter, :at-room nil}
                    {:id 2, :role :painter, :at-room nil}
                    {:id 3, :role :painter, :at-room nil}]}]

        (is (= 1
              (-> (e/rooms-needing-movers (-> state :rooms))
                count)))
        (is (= [3]
              (->> (e/rooms-needing-movers (-> state :rooms))
                (mapv :id))))

        (is (= []
              (#'sim/create-mover-assignments state)))

        (is (= [1]
              (->> (e/rooms-done-with-movers (-> state :rooms))
                (mapv :id))))

        ; next step: free completed movers
        (let [state (sim/assign-movers state)]
          (is (map?
                (sim/free-movers state)))))

      0)



    0)



  ; change state
  (testing "assign movers to rooms"
    (let [state       {:turn     3,
                       :rooms
                       (list
                         {:id                      0,
                          :role                    :room,
                          :state                   :waiting-for-movers1,
                          :moving1-time-remaining  10,
                          :painting-time-remaining 50,
                          :moving2-time-remaining  10}
                         {:id                      1,
                          :role                    :room,
                          :state                   :waiting-for-movers1,
                          :moving1-time-remaining  10,
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
                       :movers   [{:id 0, :role :mover, :at-room nil}],
                       :painters [{:id 0, :role :painter, :at-room nil}]}
          ; should assign mover 0 to room 0
          assignments (#'sim/create-mover-assignments state)]
      ;new-state (sim/assign-movers state)]
      (is (= 1
            (count assignments)))
      (is (= :removing-furniture
            (-> assignments first :room :state)))
      (is (= 0
            (-> assignments first :mover :at-room)))

      ; now state transform
      (let [new-state (#'sim/apply-moving-assignments state assignments)]

        ;1 room in work, 3 still needing movers)
        (def new-state new-state)
        (is (= 3
              (count (e/rooms-needing-movers
                       (-> new-state :rooms)))))
        ;mover is at room 0)
        (is (= 0
              (-> new-state :movers first :at-room)))
        (is (= 0
              (count (e/available-movers new-state))))

        (testing "rooms being moved"
          (is (= [0]
                (->> (sim/workers-moving new-state)
                  (mapv :id)))))

        (testing "update rooms being moved"
          (is (= 9
                (-> (sim/advance-state new-state)
                  :rooms
                  first
                  :moving1-time-remaining))))))

    0))


(deftest anonymous-fns
  (testing "anon fn"
    (is (= [{:id 1, :n 2} {:id 2, :n 3}]
          (utils/update-by-id-apply-fn
            [{:id 1 :n 1} {:id 2 :n 3}]
            1
            #(update-in % [:n] inc))))))


(deftest mult-painters-movers
  (testing "var painters"
    (def *state (atom [(sim/create-state (e/create-rooms 4) (e/create-movers 2) (e/create-painters 2))]))
    (is (= 2
          (-> @*state last :movers count))))

  (testing "(e/rooms-done-with-movers)"
    (let [rooms  [{:id                      0,
                   :role                    :room,
                   :state                   :removing-furniture,
                   :moving1-time-remaining  10,
                   :painting-time-remaining 50,
                   :moving2-time-remaining  10}
                  {:id                      1,
                   :role                    :room,
                   :state                   :removing-furniture,
                   :moving1-time-remaining  0,
                   :painting-time-remaining 50,
                   :moving2-time-remaining  10}]
          retval (e/rooms-done-with-movers rooms)]
      (is (= 1
            (count retval)))
      (is (= [1]
            (->> retval
              (map :id))))))

  (testing "update-rooms-movers"
    #_(let [newstate (utils/update-rooms-movers
                       {:old-rooms  (-> @*state last :rooms)
                        :old-movers (-> @*state last :movers)}
                       [{:room  {:id                      0,
                                 :role                    :room,
                                 :state                   :removing-furniture,
                                 :moving1-time-remaining  10,
                                 :painting-time-remaining 50,
                                 :moving2-time-remaining  10}
                         :mover {:id 0, :role :mover, :at-room 0}}])]
        (is (= :removing-furniture
              (-> newstate :old-rooms first :state))))

    (let [newstate (utils/update-rooms-movers
                     (-> @*state last)
                     [{:room  {:id                      0,
                               :role                    :room,
                               :state                   :removing-furniture,
                               :moving1-time-remaining  10,
                               :painting-time-remaining 50,
                               :moving2-time-remaining  10}
                       :mover {:id 0, :role :mover, :at-room 0}}])]
      (is (= :removing-furniture
            (-> newstate :rooms first :state))))
    0)

  (testing "update-room-movers"
    #_(is (= 1
            (utils/update-rooms-movers {:old-rooms nil :old-movers nil})))
    (is (= 1 1)))

  (testing "free-room-movers"
    (let [state    {:turn     0,
                    :rooms    [{:id                      0,
                                :role                    :room,
                                :state                   :removing-furniture
                                :moving1-time-remaining  10,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}
                               {:id                      1,
                                :role                    :room,
                                :state                   :waiting-for-movers1,
                                :moving1-time-remaining  10,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}]
                    :movers   [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room 0}],
                    :painters [{:id 0, :role :painter, :at-room nil}
                               {:id 1, :role :painter, :at-room nil}
                               {:id 2, :role :painter, :at-room nil}
                               {:id 3, :role :painter, :at-room nil}]}
          newstate (utils/free-room-movers state [0])]
      ;(println :test-free-room-movers newstate)
      (def newstate newstate)
      (is (= :waiting-for-painters
            (->> newstate
              (sp/select [:rooms 0 :state])
              first)))
      (is (= nil
            (->> newstate
              (sp/select [:movers 1 :at-room])
              first)))
      (is (= :waiting-for-painters
            (->> newstate
              (sp/select [:rooms 0 :state])
              first))))
    (is (= 1 1)))
  0)

(deftest add-painting
  (testing "basics painting"
    (let [state {:turn     26,
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
                             :moving1-time-remaining  0,
                             :painting-time-remaining 10,
                             :moving2-time-remaining  10}
                            {:id                      3,
                             :role                    :room,
                             :state                   :waiting-for-painters,
                             :moving1-time-remaining  0,
                             :painting-time-remaining 10,
                             :moving2-time-remaining  10}],
                 :movers   [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room nil}],
                 :painters [{:id 0, :role :painter, :at-room nil}
                            {:id 1, :role :painter, :at-room nil}
                            {:id 2, :role :painter, :at-room nil}
                            {:id 3, :role :painter, :at-room nil}]}]
      (is (= 1 1))
      (is (= [0 1 2 3]
            (->> (e/rooms-needing-painters state)
              (mapv :id))))
      (is (= [:painting :painting :painting :painting]
            (->> (#'sim/create-painter-assignments2 state)
              (map #(-> % :room :state)))))
      (is (= [0 1 2 3]
            (->> (#'sim/create-painter-assignments2 state)
              (map #(-> % :painter :at-room)))))
      (is (= [0 1 2 3]
            (->> (sim/assign-painters state)
              :painters
              (mapv #(-> % :at-room))
              sort)))

      0))
  ; now test for painting done
  (testing "basics painting"
    (let [state {:turn     26,
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
                             :moving1-time-remaining  0,
                             :painting-time-remaining 10,
                             :moving2-time-remaining  10}
                            {:id                      3,
                             :role                    :room,
                             :state                   :waiting-for-painters,
                             :moving1-time-remaining  0,
                             :painting-time-remaining 10,
                             :moving2-time-remaining  10}],
                 :movers   [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room nil}],
                 :painters [{:id 0, :role :painter, :at-room nil}
                            {:id 1, :role :painter, :at-room nil}
                            {:id 2, :role :painter, :at-room nil}
                            {:id 3, :role :painter, :at-room nil}]}]
      (is (= 1 1))
      (is (= [0 1 2 3]
            (->> (e/rooms-needing-painters state)
              (mapv :id))))
      (is (= [:painting :painting :painting :painting]
            (->> (#'sim/create-painter-assignments2 state)
              (map #(-> % :room :state)))))
      (is (= [0 1 2 3]
            (->> (#'sim/create-painter-assignments2 state)
              (map #(-> % :painter :at-room)))))
      (is (= [0 1 2 3]
            (->> (sim/assign-painters state)
              :painters
              (mapv #(-> % :at-room))
              sort)))

      ; now test for painting done
      0))
  (testing "painting done"
    (let [state {:turn     26,
                 :rooms    [{:id                      0,
                             :role                    :room,
                             :state                   :painting
                             :moving1-time-remaining  0,
                             :painting-time-remaining 0,
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
                             :moving1-time-remaining  0,
                             :painting-time-remaining 10,
                             :moving2-time-remaining  10}
                            {:id                      3,
                             :role                    :room,
                             :state                   :waiting-for-painters,
                             :moving1-time-remaining  0,
                             :painting-time-remaining 10,
                             :moving2-time-remaining  10}],
                 :movers   [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room nil}],
                 :painters [{:id 0, :role :painter, :at-room 0}
                            {:id 1, :role :painter, :at-room nil}
                            {:id 2, :role :painter, :at-room nil}
                            {:id 3, :role :painter, :at-room nil}]}]
      (is (= 1 1))
      (is (= [0]
            (->> (e/rooms-done-with-painters (-> state :rooms))
              (map :id))))

      (let [newstate (sim/free-painters state)]
        (def newstate newstate)

        (is (= nil
              (->> newstate
                :painters
                first
                :at-room))))

      (testing "advance state"
        (let [newstate2 (sim/advance-state newstate)]
          (def newstate2 newstate2)

          (is (= :waiting-for-movers2
                (->> newstate2
                  :rooms
                  first
                  :state)))))
      0))

  (testing "mult painters"
    (let [state {:turn     26,
                 :rooms    [{:id                      0,
                             :role                    :room,
                             :state                   :painting
                             :moving1-time-remaining  0,
                             :painting-time-remaining 0,
                             :moving2-time-remaining  10}
                            {:id                      1,
                             :role                    :room,
                             :state                   :painting
                             :moving1-time-remaining  0,
                             :painting-time-remaining 10,
                             :moving2-time-remaining  10}
                            {:id                      2,
                             :role                    :room,
                             :state                   :painting
                             :moving1-time-remaining  0,
                             :painting-time-remaining 10,
                             :moving2-time-remaining  10}
                            {:id                      3,
                             :role                    :room,
                             :state                   :waiting-for-painters,
                             :moving1-time-remaining  0,
                             :painting-time-remaining 10,
                             :moving2-time-remaining  10}],
                 :movers   [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room nil}],
                 :painters [{:id 0, :role :painter, :at-room 0}
                            {:id 1, :role :painter, :at-room 1}
                            {:id 2, :role :painter, :at-room 2}
                            {:id 3, :role :painter, :at-room nil}]}]
      (let [newstate (-> state
                       sim/assign-movers
                       sim/free-movers
                       sim/assign-painters
                       sim/free-painters
                       sim/advance-state
                       sim/next-turn)]
        (def newstate newstate)

        ; first room done
        (is (= nil
              (->> newstate
                :painters
                first
                :at-room)))

        ; second room 2: 9 painting remaining
        (is (= 9
              (->> newstate
                :rooms
                second
                :painting-time-remaining)))
        0)
      0)))

(comment
  (let [newstate (-> newstate
                   sim/assign-movers
                   sim/free-movers
                   sim/assign-painters
                   sim/free-painters
                   sim/advance-state
                   sim/next-turn)]
    (def newstate newstate)
    newstate)
  0)


(deftest next-actions
  (loop [n     40
         state {:turn     10,
                :rooms    [{:id                      0,
                            :role                    :room,
                            :state                   :removing-furniture,
                            :moving1-time-remaining  0,
                            :painting-time-remaining 10,
                            :moving2-time-remaining  10}
                           {:id                      1,
                            :role                    :room,
                            :state                   :removing-furniture,
                            :moving1-time-remaining  1,
                            :painting-time-remaining 10,
                            :moving2-time-remaining  10}
                           {:id                      2,
                            :role                    :room,
                            :state                   :waiting-for-movers1,
                            :moving1-time-remaining  10,
                            :painting-time-remaining 10,
                            :moving2-time-remaining  10}
                           {:id                      3,
                            :role                    :room,
                            :state                   :waiting-for-movers1,
                            :moving1-time-remaining  10,
                            :painting-time-remaining 10,
                            :moving2-time-remaining  10}],
                :movers   (vector {:id 0, :role :mover, :at-room 0} {:id 1, :role :mover, :at-room 1}),
                :painters (vector {:id 0, :role :painter, :at-room nil}
                            {:id 1, :role :painter, :at-room nil}
                            {:id 2, :role :painter, :at-room nil}
                            {:id 3, :role :painter, :at-room nil})}]

    (let [newstate (-> state
                     sim/assign-movers)]
      (def newstate newstate)
      (is (not (nil? (-> newstate :rooms first :state)))))

    (let [newstate (-> state
                     sim/assign-movers
                     sim/free-movers
                     sim/assign-painters
                     sim/free-painters
                     sim/advance-state
                     sim/next-turn)]
      (def newstate newstate)

      (is (not (nil? (-> newstate :rooms first :state))))

      (is (= 1 1))
      (println :test :next-actions :newstate (utils/pp-str newstate))
      (if (pos? n)
        (recur (dec n) newstate))))

  0)

(deftest done?
  (let [
        state {:turn     10,
               :rooms    [{:id                      0,
                           :role                    :room,
                           :state                   :finished
                           :moving1-time-remaining  0,
                           :painting-time-remaining 10,
                           :moving2-time-remaining  10}
                          {:id                      1,
                           :role                    :room,
                           :state                   :finished
                           :moving1-time-remaining  1,
                           :painting-time-remaining 10,
                           :moving2-time-remaining  10}
                          {:id                      2,
                           :role                    :room,
                           :state                   :finished
                           :moving1-time-remaining  10,
                           :painting-time-remaining 10,
                           :moving2-time-remaining  10}
                          {:id                      3,
                           :role                    :room,
                           :state                   :finished
                           :moving1-time-remaining  10,
                           :painting-time-remaining 10,
                           :moving2-time-remaining  10}],
               :movers   (vector {:id 0, :role :mover, :at-room 0} {:id 1, :role :mover, :at-room 1}),
               :painters (vector {:id 0, :role :painter, :at-room nil}
                           {:id 1, :role :painter, :at-room nil
                            {:id 2, :role :painter, :at-room nil}
                            {:id 3, :role :painter, :at-room nil}})}]

    (is (true? (e/all-rooms-finished? state)))
    (is (false? (e/all-rooms-finished? (-> state
                                         (assoc-in [:rooms 0 :state] :waiting-for-movers2)))))
    0)
  0)

(deftest painter-present
  (let [state {:turn     19,
               :rooms    [{:id                      0,
                           :role                    :room,
                           :state                   :waiting-for-painters,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}
                          {:id                      1,
                           :role                    :room,
                           :state                   :waiting-for-painters,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}
                          {:id                      2,
                           :role                    :room,
                           :state                   :removing-furniture,
                           :moving1-time-remaining  8,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}
                          {:id                      3,
                           :role                    :room,
                           :state                   :waiting-for-movers1,
                           :moving1-time-remaining  10,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}],
               :movers   [{:id 0, :role :mover, :at-room 2}],
               :painters [{:id 0, :role :painter, :at-room 0}]}]
    (is (true? (sim/room-has-painter state 0)))
    (is (false? (sim/room-has-painter state 1)))
    (is (false? (sim/room-has-painter state 2)))

    ; room 0 should flip to :painting
    (is (= :painting
          (->> (sim/rooms-needs-painter-already-there state)
            first
            :state)))
    ; not others
    (is (= :waiting-for-painters
          (->> (sim/rooms-needs-painter-already-there state)
            second
            :state)))

    (let [newstate (sim/advance-state state)]
      (def newstate newstate)
      (is (= :painting
            (-> newstate :rooms first :state)))))


  (let [state    {:turn     10,
                  :rooms    [{:id                      0,
                              :role                    :room,
                              :state                   :removing-furniture,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  10}
                             {:id                      1,
                              :role                    :room,
                              :state                   :removing-furniture,
                              :moving1-time-remaining  0,
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
                              :moving2-time-remaining  10}
                             {:id                      4,
                              :role                    :room,
                              :state                   :waiting-for-movers1,
                              :moving1-time-remaining  10,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  10}
                             {:id                      5,
                              :role                    :room,
                              :state                   :removing-furniture,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  10}
                             {:id                      6,
                              :role                    :room,
                              :state                   :removing-furniture,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  10}
                             {:id                      7,
                              :role                    :room,
                              :state                   :removing-furniture,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  10}],
                  :movers   [{:id 0, :role :mover, :at-room 0} {:id 1, :role :mover, :at-room 1}],
                  :painters [{:id 0, :role :painter, :at-room 7} {:id 1, :role :painter, :at-room 6} {:id 2, :role :painter, :at-room 5}]}
        newstate (sim/simulate-turn state)]


    ; room 0 should still be waiting
    (def newstate newstate)
    (is (= :waiting-for-painters
          (->> (sim/simulate-turn newstate)
            :rooms
            first
            :state)))
    ; room 7 should flip
    (is (= [5 6 7]
          (->> (sim/rooms-needs-painter-already-there newstate)
            (filter #(= :painting (:state %)))
            (mapv :id))))
    ; not others
    #_(is (= :waiting-for-painters
            (->> (sim/rooms-needs-painter-already-there state)
              second
              :state)))

    #_(let [newstate (sim/advance-state state)]
        (def newstate newstate)
        (is (= :painting
              (-> newstate :rooms first :state)))))
  0)




(deftest painting2

  (let [state {:turn     10,
               :rooms    [{:id                      6,
                           :role                    :room,
                           :state                   :removing-furniture,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}
                          {:id                      7,
                           :role                    :room,
                           :state                   :removing-furniture,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}],
               :movers   [{:id 0, :role :mover, :at-room 0} {:id 1, :role :mover, :at-room 1}],
               :painters [{:id 0, :role :painter, :at-room 7} {:id 1, :role :painter, :at-room 6} {:id 2, :role :painter, :at-room 5}]}]
    (is (= [6 7]
          (->> (sim/rooms-needs-painter-already-there state)
            (mapv :id)))))

  (let [state {:turn     10,
               :rooms    [{:id                      0,
                           :role                    :room,
                           :state                   :removing-furniture,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}
                          {:id                      1,
                           :role                    :room,
                           :state                   :removing-furniture,
                           :moving1-time-remaining  0,
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
                           :moving2-time-remaining  10}
                          {:id                      4,
                           :role                    :room,
                           :state                   :waiting-for-movers1,
                           :moving1-time-remaining  10,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}
                          {:id                      5,
                           :role                    :room,
                           :state                   :removing-furniture,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}
                          {:id                      6,
                           :role                    :room,
                           :state                   :removing-furniture,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}
                          {:id                      7,
                           :role                    :room,
                           :state                   :removing-furniture,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 50,
                           :moving2-time-remaining  10}],
               :movers   [{:id 0, :role :mover, :at-room 0} {:id 1, :role :mover, :at-room 1}],
               :painters [{:id 0, :role :painter, :at-room 7} {:id 1, :role :painter, :at-room 6} {:id 2, :role :painter, :at-room 5}]}]
    (let [newstate (sim/simulate-turn state)]
      (def newstate newstate)
      ; room must still be waiting!
      (is (= :waiting-for-painters
            (-> newstate :rooms first :state)))))


  0)


#_(deftest complete-sim-1
    ; problem: room stuck in :waiting for painters, even though painter is already there
    (let [state  (sim/create-state (e/create-rooms 4) (e/create-movers 1) (e/create-painters 1))
          states (sim/simulate-until-done state)]
      (def states states)
      ;(is (= 221))
      (is (= 202
            (count states)))))
;XXX
;(reset! sim/*state states))


(deftest parking-painters
  (let [state {:turn     26,
               :rooms    [{:id                      0,
                           :role                    :room,
                           :state                   :waiting-for-movers1
                           :moving1-time-remaining  10,
                           :painting-time-remaining 10,
                           :moving2-time-remaining  10}
                          {:id                      1,
                           :role                    :room,
                           :state                   :waiting-for-painters,
                           :moving1-time-remaining  0,
                           :painting-time-remaining 10,
                           :moving2-time-remaining  10}],
               :movers   [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room nil}],
               :painters [{:id 0, :role :painter, :at-room nil}
                          {:id 1, :role :painter, :at-room nil}
                          {:id 2, :role :painter, :at-room nil}
                          {:id 3, :role :painter, :at-room nil}]}]
    ; old mode
    (is (= [1]
          (->> (e/rooms-needing-painters state)
            (mapv :id))))
    (is (= [0 1]
          (->> (e/rooms-needing-painters state {:strict false})
            (mapv :id)))))

  (testing "painting already there"
    (let [state    {:turn     21440,
                    :rooms    [{:id                      0,
                                :role                    :room,
                                :state                   :waiting-for-painters,
                                :moving1-time-remaining  0,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}
                               {:id                      1,
                                :role                    :room,
                                :state                   :waiting-for-painters,
                                :moving1-time-remaining  0,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}
                               {:id                      2,
                                :role                    :room,
                                :state                   :waiting-for-painters,
                                :moving1-time-remaining  0,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}
                               {:id                      3,
                                :role                    :room,
                                :state                   :waiting-for-painters,
                                :moving1-time-remaining  0,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}
                               {:id                      4,
                                :role                    :room,
                                :state                   :waiting-for-painters,
                                :moving1-time-remaining  0,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}
                               {:id                      5,
                                :role                    :room,
                                :state                   :waiting-for-painters,
                                :moving1-time-remaining  0,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}
                               {:id                      6,
                                :role                    :room,
                                :state                   :waiting-for-painters,
                                :moving1-time-remaining  0,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}
                               {:id                      7,
                                :role                    :room,
                                :state                   :waiting-for-painters,
                                :moving1-time-remaining  0,
                                :painting-time-remaining 50,
                                :moving2-time-remaining  10}],
                    :movers   [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room nil}],
                    :painters [{:id 0, :role :painter, :at-room 7} {:id 1, :role :painter, :at-room 6} {:id 2, :role :painter, :at-room 5}]}
          newstate (sim/advance-state state)]
      1
      (def newstate newstate)
      (is (map? newstate))
      (is (= [5 6 7]
            (->> (e/state->rooms newstate)
              (filter #(= :painting (:state %)))
              (mapv :id))))
      (is (= :waiting-for-painters
            (-> newstate :rooms first :state)))))

  (comment
    (-> sim/*state deref last)
    0)


  0)

(deftest moving-termination
  (let [state    {:turn     76,
                  :rooms    [{:id                      0,
                              :role                    :room,
                              :state                   :waiting-for-painters,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  25}
                             {:id                      1,
                              :role                    :room,
                              :state                   :waiting-for-painters,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  25}
                             {:id                      2,
                              :role                    :room,
                              :state                   :waiting-for-painters,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  25}
                             {:id                      3,
                              :role                    :room,
                              :state                   :waiting-for-painters,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  25}
                             {:id                      4,
                              :role                    :room,
                              :state                   :removing-furniture,
                              :moving1-time-remaining  1,
                              :painting-time-remaining 50,
                              :moving2-time-remaining  25}
                             {:id                      5,
                              :role                    :room,
                              :state                   :waiting-for-movers2,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 0,
                              :moving2-time-remaining  25}
                             {:id                      6,
                              :role                    :room,
                              :state                   :waiting-for-movers2,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 0,
                              :moving2-time-remaining  25}
                             {:id                      7,
                              :role                    :room,
                              :state                   :waiting-for-movers2,
                              :moving1-time-remaining  0,
                              :painting-time-remaining 0,
                              :moving2-time-remaining  25}],
                  :movers   [{:id 0, :role :mover, :at-room 4} {:id 1, :role :mover, :at-room nil}],
                  :painters [{:id 0, :role :painter, :at-room nil}
                             {:id 1, :role :painter, :at-room nil}
                             {:id 2, :role :painter, :at-room nil}]}
        newstate (sim/simulate-turn state)]
    (def newstate newstate)
    ; room 4 time remaining 0
    (is (= [0]
          (sp/select [:rooms 4 :moving1-time-remaining] newstate)))
    ; room 4 state should be still :removing-furniture
    (is (= :removing-furniture
          (-> (sp/select [:rooms 4 :state] newstate) first))))
  0)


(comment
  (sp/select [:rooms 4 :state] newstate)
  (sp/select [:rooms 4 :moving1-time-remaining] newstate)
  (-> @sim/*state (nth 51))
  0)

#_(deftest mover-stuck1)

(comment
  (sp/select [:movers 0 :at-room] newstate)
  (sp/select [:movers 1 :at-room] newstate)

  0)

(deftest zeroorneg
  (is (true? (e/zero-or-neg? 0)))
  (is (true? (e/zero-or-neg? -1)))
  (is (false? (e/zero-or-neg? 1))))

(deftest painting-stuck-300
  (let [state {:turn 331,
               :rooms [{:id 0,
                        :role :room,
                        :state :finished,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 0}
                       {:id 1,
                        :role :room,
                        :state :finished,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 0}
                       {:id 2,
                        :role :room,
                        :state :finished,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 0}
                       {:id 3,
                        :role :room,
                        :state :finished,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 0}
                       {:id 4,
                        :role :room,
                        :state :finished,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 0}
                       {:id 5,
                        :role :room,
                        :state :finished,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 0}
                       {:id 6,
                        :role :room,
                        :state :finished,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 0}
                       {:id 7,
                        :role :room,
                        :state :painting,
                        :moving1-time-remaining 0,
                        :painting-time-remaining -322,
                        :moving2-time-remaining 25}],
               :movers [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room nil}],
               :painters [{:id 0, :role :painter, :at-room 7} {:id 1, :role :painter, :at-room 7} {:id 2, :role :painter, :at-room 7}]}
        newstate (sim/simulate-turn state)]
    (def newstate newstate)
    ; Turn 300: some things going
    ;wrong: Room 7 has 3 painters,
    ;decrementing three times. Time
    ;remaining is -322;
    ;
    ;- how'd it go negative?
    ;- why isn't state changing?
    ;- why are there three painters
    ;there?
    ; should be done painting
    (is (= [:waiting-for-movers2] (sp/select [:rooms 7 :state] newstate)))
    ; should only be no painters
    (is (= [nil nil nil] (sp/select [:painters sp/ALL :at-room] newstate)))

    ; are painters done
    (is (= []
          (e/rooms-done-with-painters (-> newstate :rooms))))))

(comment
  (e/rooms-done-with-painters (-> newstate :rooms))
  ; => []

  0)

(deftest painting-double-booked1
  (let [state {:turn 128,
               :rooms [{:id 0,
                        :role :room,
                        :state :finished,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 0}
                       {:id 1,
                        :role :room,
                        :state :finished,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 0}
                       {:id 2,
                        :role :room,
                        :state :restoring-furniture,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 1}
                       {:id 3,
                        :role :room,
                        :state :waiting-for-movers2,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 25}
                       {:id 4,
                        :role :room,
                        :state :waiting-for-movers2,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 0,
                        :moving2-time-remaining 25}
                       {:id 5,
                        :role :room,
                        :state :painting,
                        :moving1-time-remaining 0,
                        :painting-time-remaining 24,
                        :moving2-time-remaining 25}
                       {:id 6,
                        :role :room,
                        :state :removing-furniture,
                        :moving1-time-remaining 1,
                        :painting-time-remaining 50,
                        :moving2-time-remaining 25}
                       {:id 7,
                        :role :room,
                        :state :waiting-for-movers1,
                        :moving1-time-remaining 25,
                        :painting-time-remaining 50,
                        :moving2-time-remaining 25}],
               :movers [{:id 0, :role :mover, :at-room 2} {:id 1, :role :mover, :at-room 6}],
               :painters [{:id 0, :role :painter, :at-room 6}
                          {:id 1, :role :painter, :at-room nil}
                          {:id 2, :role :painter, :at-room 5}]}
        newstate (sim/simulate-turn state)]
    (def state state)
    (def newstate newstate)
    ; Turn 127 -> 128: Room 6:
    ;- two painters assigned to same
    ;room
    (is (empty?
          (#'sim/create-painter-assignments2 state)))
    (is (= [6 5]
          (sp/select [:painters sp/ALL :at-room] newstate)))))

(comment
  (sp/select [:painters sp/ALL :at-room] newstate))
0