(ns s01-rules-engine
  (:require
    [nextjournal.clerk :as clerk]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [odoyle.rules :as o]))


; https://github.com/oakes/play-cljc-examples/blob/master/dungeon-crawler/src/dungeon_crawler/session.cljc


; ## make first rules

(def rules
  (o/ruleset
    {::steve
     [:what
      [::this ::that 0]]

     ::supervisor
     [:what
      [::supervisor ::x x]
      [::supervisor ::y y]]}))

; create session and add rule
(def *session
  (atom (reduce o/add-rule (o/->session) rules)))

;(o/insert *session ::steve ::supervisor true)
;(o/insert *session ::supervisor ::x 20)
;(o/insert *session ::supervisor ::y 30)

(swap! *session
  (fn [session]
    (-> session
      (o/insert ::supervisor ::x 20)
      (o/insert ::supervisor ::y 30)
      (o/fire-rules))))

(o/query-all @*session)