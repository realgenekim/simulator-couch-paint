(ns genek.sim
  (:require
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [odoyle.rules :as o]))

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

(def *session
  (atom (reduce o/add-rule (o/->session) rules)))


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

(defn add-movers
  []
  (let [eavs (for [i (range 10)]
               [(+ 10 i) {::role :mover
                          ::name (format "mover-%d" i)
                          ::x    100
                          ::y    100}])]
    (doall
      (for [[i eav] eavs]
        (do
          (println i eav)
          (swap! *session
            (fn [session]
              (-> session
                (o/insert i eav)
                (o/fire-rules))))))))
  0)

(defn add-painters
  []
  (let [eavs (for [i (range 10)]
               [(+ 20 i) {::role :painter
                          ::name (format "painter-%d" i)
                          ::x    100
                          ::y    100}])]
    (doall
      (for [[i eav] eavs]
        (do
          (println i eav)
          (swap! *session
            (fn [session]
              (-> session
                (o/insert i eav)
                (o/fire-rules))))))))
  0)

(add-movers)
(add-painters)

