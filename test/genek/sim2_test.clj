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

