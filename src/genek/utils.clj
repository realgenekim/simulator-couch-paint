(ns genek.utils)

(defn pp-str
  [x]
  (with-out-str
    (clojure.pprint/pprint x)))
