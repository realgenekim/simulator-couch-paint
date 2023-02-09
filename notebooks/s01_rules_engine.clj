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

     ::actor
     [:what
      [id ::role role]
      [id ::name name]
      [id ::x x]
      [id ::y y]]

     ::supervisor
     [:what
      [::supervisor ::x x]
      [::supervisor ::y y]
      [::supervisor :supervisor/name nm]
      ,]}))

(def *latest-id (atom 0))

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

(defn insert-entity!
  [sess a v]
  (let [id @*latest-id]
    (swap! *latest-id inc)
    (println :insert-entity id a v)

    (swap! *session
      (fn [session]
        (-> session
          (o/insert id
                {::role :supervisor
                 ::name v
                 ::x 100
                 ::y 100})
          (o/fire-rules))))))



(defn add-supervisors
  " add steve and gene"
  []
  (insert-entity! *session nil "steve")
  (insert-entity! *session nil "gene"))

(add-supervisors)

#_(swap! *session
    (fn [session]
      (-> session
        (o/insert 1 ::role ::supervisor)
        (o/insert 1 ::name "steve")
        (o/insert 1 ::y 30)
        (o/insert 1 ::x 50)
        (o/fire-rules))))



(o/query-all @*session)
(o/query-all @*session ::supervisor)
(o/query-all @*session ::actor)
