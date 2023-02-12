(ns s01-rules-engine
  (:require
    [nextjournal.clerk :as clerk]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [genek.sim :as sim]
    [odoyle.rules :as o]))


; https://github.com/oakes/play-cljc-examples/blob/master/dungeon-crawler/src/dungeon_crawler/session.cljc


; ## make first rules

; create session and add rule

;(o/insert *session ::steve ::supervisor true)
;(o/insert *session ::supervisor ::x 20)
;(o/insert *session ::supervisor ::y 30)





(o/query-all @sim/*session)
(o/query-all @sim/*session ::sim/supervisor)
(o/query-all @sim/*session ::sim/actor)
