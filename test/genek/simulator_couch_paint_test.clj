(ns genek.simulator-couch-paint-test
  (:require [clojure.test :refer :all]
            [genek.sim :as sim]
            [genek.simulator-couch-paint :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))


(def *session nil)

(deftest sim

  ; [{:id 0, :role :supervisor, :name "steve", :x 100, :y 100}
  ; {:id 1, :role :supervisor, :name "gene", :x 100, :y 100}
  ; {:id 10, :role :mover, :name "mover-0", :x 100, :y 100}
  ; {:id 11, :role :mover, :name "mover-1", :x 100, :y 100}
  ; {:id 12, :role :mover, :name "mover-2", :x 100, :y 100}
  ; {:id 20, :role :painter, :name "painter-0", :x 100, :y 100}
  ; {:id 21, :role :painter, :name "painter-1", :x 100, :y 100}
  ; {:id 22, :role :painter, :name "painter-2", :x 100, :y 100}]

  (testing "first query"
    (let [q (sim/q)]
      (is (sequential? q))
      (is (sequential? q))))

  0)

(comment
  (def qr (sim/q))
  (group-by :role qr)
  0)
