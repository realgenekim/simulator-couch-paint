(ns s02-recursive-search
  (:require
    [nextjournal.clerk :as clerk]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [genek.sim2 :as sim]))

;(remove-ns 's02-recursive-search)

;; ## State

(sim/init-state!)
(clerk/table @sim/*state)

;; assign mover to room
(def newstate (sim/assign-movers (-> @sim/*state last)))


(def newstates
  ;(sim/simulate-until-done (-> @sim/*state last) {:maxturns 200})
  (sim/simulate-until-done (-> @sim/*state last) {:maxturns 500}))

(reset! sim/*state newstates)

; ## Results
;
; - FIFO: 257 turns
; - LIFO: 173 turns -- same, because painters never wait...

#_(doseq [i (range 200)]
    (let [newstate (-> (-> @sim/*state last)
                     sim/assign-movers
                     sim/free-movers
                     sim/assign-painters
                     sim/free-painters
                     sim/advance-state)]
      (sim/next-turn! sim/*state newstate)))
  ;(let [newstate (sim/assign-movers (-> @sim/*state last))]
  ;  (sim/next-turn! sim/*state newstate)))


