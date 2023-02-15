(ns s02-recursive-search
  (:require
    [nextjournal.clerk :as clerk]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [genek.sim2 :as sim]))

;(remove-ns 's02-recursive-search)

;; ## Rooms

(sim/create-room 1)

(clerk/table
  sim/rooms)

;; ## Movers

(clerk/table sim/movers)

;; ## Painters

(clerk/table sim/painters)

;; ## State

(clerk/table @sim/*state)

;; assign mover to room
(def newstate (sim/assign-available-movers (-> @sim/*state last)))

;; increment turn

(sim/next-turn! sim/*state newstate)
@sim/*state
