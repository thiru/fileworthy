;; ## Summary
;;
;; The home page.
;;
(ns fileworthy.web.routes.home
  (:require
            [clojure.string :as string]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.http-response :as hr]

            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [fileworthy.app :as app]
            [fileworthy.web.routes.template :refer :all]))

(defn get-home-page
  "Home page.

  TODO"
  [req]
  (template-page
    req
    "Home"
    [:h1 "TODO"]))
