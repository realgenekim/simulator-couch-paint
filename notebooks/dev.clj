(ns dev
  (:require
    [nextjournal.clerk :as clerk]
    [portal.api :as p]))
;[flow-storm.api :as fs-api]))

;; start Clerk's buit-in webserver on the default port 7777, opening the browser when done
;(clerk/serve! {:browse? true})

;; either call `clerk/show!` explicitly
;(clerk/show! "notebooks/rule_30.clj")

;; or let Clerk watch the given `:paths` for changes
(clerk/serve! {:watch-paths ["notebooks"]
               :browse? true})

(comment
  (clerk/serve! {:watch-paths ["notebooks" "src"]
                 :browse? true})

  (clerk/halt!)
  (clerk/clear-cache!)


  (do
    ;(plog/configure-logging!)
    ;(def po (p/open))
    (def po (p/open {:launcher :intellij}))
    (add-tap #'p/submit)

    (tap> {:a 1}))

  (def po (p/open {:launcher :intellij}))
  (def po (p/open))

  (do
    (fs-api/local-connect)
    0)

  ;#rtrace (reduce + (map inc (range 10)))

  0)

