;; ## Summary
;;
;; Initial namespace loaded when using a REPL (e.g. using `clj`).
;;
(ns user
  (:require
            [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [clojure.reflect :as reflect]
            [clojure.repl :refer :all]
            [clojure.string :as string]

            [cheshire.core :as json]

            [thiru.filedb :as filedb]
            [thiru.fsreload :as reload]
            [thiru.logging :refer :all]
            [thiru.repl :as repl]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [fileworthy.app :as app]
            [fileworthy.core.users :as users]
            [fileworthy.web.server :as server]))

;; We only want to start the watcher once.
(defonce watch-started?
  (reload/start-watch!))
