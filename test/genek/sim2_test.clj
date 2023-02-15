(ns genek.sim2-test
  (:require
    [clojure.test :refer :all]
    [genek.sim2 :as sim]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest primitives
  (testing "inc turn: base case"
    (let [newturn (sim/create-state [] [] [])]
      (is (= 0 (:turn newturn)))))
  (testing "inc turn: base case"
    (let [newturn (sim/create-state {:turn 1}  [] [] [])]
      (is (= 2 (:turn newturn))))))

(deftest states
  (def *state (atom [(sim/create-state sim/rooms sim/movers sim/painters)]))
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
    (let [ms [{:id 1 :val 123}
              {:id 2 :val 456}]
          newms (sim/update-by-id ms {:id 1 :val 999})
          newms2 (sim/update-by-id ms {:id 2 :val 997})
          newms3 (sim/update-by-id ms {:id 3 :val 997})]
      (is (= 999
            (-> newms first :val)))
      (is (= 997
            (-> newms2 second :val)))
      (is (= ms newms3)))
    0))

(deftest match-resource
  (def *state (atom [(sim/create-state sim/rooms sim/movers sim/painters)]))

  (testing "needs movers/painters"
    (is (not= true
          (get sim/state-needs-mover? :initial)))
    (is (= true
          (get sim/state-needs-mover? :waiting-for-movers1)))
    (is (= true
          (get sim/state-needs-mover? :waiting-for-movers2))))

  (testing "room needs movers"
    (let [needs-movers (sim/rooms-needing-movers (-> @*state last :rooms))]
      (is (= 4
            (count needs-movers)))))

  (testing "next-turn-fn!"
    (sim/next-turn-fn! *state sim/increment-state)
    (is (= 2 (count @*state))))

  (testing "available movers"
    (let [available-movers (sim/available-movers (-> @*state last :movers))]
      (is (= 1
            (count available-movers)))))



  ; change state
  (testing "assign movers to rooms"
    (let [new-state (sim/assign-available-movers (-> @*state last))
          rooms-still-needs-movers (sim/rooms-needing-movers
                                     (-> new-state :rooms))]
      (def new-state new-state)
      ; 1 room in work, 3 still needing movers
      (is (= 3
            (count rooms-still-needs-movers)))
      ; mover is at room 0
      (is (= 0
            (-> new-state :movers first :at-room)))
      (is (= 0
            (count (sim/available-movers (-> new-state :movers)))))

      (testing "rooms being moved"
        (is (= [0]
              (sim/rooms-being-moved (-> new-state))))))
    0





    0))

