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
  (is (= 1 (count @*state)))

  (sim/next-turn! *state)
  (is (= 2 (count @*state)))

  (sim/next-turn-fn! *state sim/increment-state)
  (is (= 3 (count @*state)))
  (is (= 2 (->> @*state last :turn)))

  (sim/next-turn-fn! *state sim/increment-state)
  (is (= 4 (count @*state)))
  (is (= 3 (->> @*state last :turn)))


  0)

