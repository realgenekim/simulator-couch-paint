(ns genek.entities
  (:require
    [clojure.spec.alpha :as s]))


; mover or painter record
(s/def ::s-record
  (s/keys :req-un [::id]))
(s/def ::s-records
  (s/coll-of ::s-record))
