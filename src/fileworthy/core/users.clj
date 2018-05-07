;; ## Summary
;;
;; Contains functionality around registered users.
;;
(ns fileworthy.core.users
  (:require
            [clojure.string :as str]
            [clojure.pprint :refer :all]

            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [fileworthy.app :as app]
            [fileworthy.web.routes.template :refer :all]))

(defn get-all
  "TODO: docs"
  [info-map]
  {:id 1 :name "Thiru"})
