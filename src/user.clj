;; ## Summary
;;
;; Initial namespace loaded when using a REPL (e.g. using `clj`).
;;
(ns user
  (:require
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [clojure.reflect :as reflect]
            [clojure.repl :refer :all]
            [clojure.string :as string]

            [cheshire.core :as json]

            [glu.ednfilestore :as efs]
            [glu.fsreload :as reload]
            [glu.logging :refer :all]
            [glu.repl :as repl]
            [glu.reporting :refer :all]
            [glu.utils :refer :all]

            [fileworthy.app :as app]
            [fileworthy.core.users :as users]
            [fileworthy.web.server :as server]))

;; We only want to start the watcher once.
(defonce watch-started?
  (reload/start-watch!))
