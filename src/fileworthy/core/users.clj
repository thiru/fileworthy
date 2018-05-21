;; ## Summary
;;
;; Contains functionality around registered users.
;;
(ns fileworthy.core.users
  (:require
            [clojure.string :as str]
            [clojure.pprint :refer :all]

            [glu.logging :refer :all]
            [glu.reporting :refer :all]
            [glu.utils :refer :all]

            [fileworthy.app :as app]
            [fileworthy.web.routes.template :refer :all]))

(defn get-all
  "TODO: docs"
  [info-map]
  {:id 1 :name "Thiru"})
