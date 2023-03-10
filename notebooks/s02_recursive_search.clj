(ns s02-recursive-search
  (:require
    [nextjournal.clerk :as clerk]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [genek.sim2 :as sim]))

;(remove-ns 's02-recursive-search)

;; ## State

(clerk/table @sim/*state)

;; assign mover to room
(def newstate (sim/assign-available-movers (-> @sim/*state last)))

;; increment turn

(sim/next-turn! sim/*state newstate)

(doseq [i (range 5)]
  (let [newstate (-> (-> @sim/*state last)
                   sim/assign-available-movers
                   sim/free-completed-movers
                   sim/advance-state)]
    (sim/next-turn! sim/*state newstate)))
  ;(let [newstate (sim/assign-available-movers (-> @sim/*state last))]
  ;  (sim/next-turn! sim/*state newstate)))

(-> @sim/*state last)
