(ns genek.utils
  (:require
    [clojure.spec.alpha :as s]
    [genek.entities :as e]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]))

(defn pp-str
  [x]
  (with-out-str
    (clojure.pprint/pprint x)))

; https://stackoverflow.com/questions/40370240/easy-way-to-change-specific-list-item-in-list

(defn list-update-in [l i x]
  " input: list l, index, item "
  (let [newlist (take i l)
        newlist (concat newlist (list x))
        newlist (concat newlist (drop (+ 1 i) l))]
    newlist))

(>defn update-by-id
  " input: sequence of maps (rooms, movers, painters), and new record with {:id } to replace record
    output: seq of maps, with replace record "
  [ms newmap] [::e/s-records map? => ::e/s-records]
  (->> ms
    (map (fn [m]
           (if (= (:id m) (:id newmap))
             newmap
             m)))))

(>defn update-by-id-apply-fn
  " input: sequence of maps (rooms, movers, painters), and new record with {:id } to apply f
    output: seq of maps, with replace record "
  [ms id f] [::e/s-records integer? fn? => ::e/s-records]
  (->> ms
    (map (fn [m]
           (if (= (:id m) id)
             (-> m f)
             m)))))

(comment
  (update-by-id-apply-fn
    [{:id 1 :n 1} {:id 2 :n 3}]
    1
    #(update-in % [:n] inc))
  0)