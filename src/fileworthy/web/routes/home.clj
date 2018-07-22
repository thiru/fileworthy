(ns fileworthy.web.routes.home
  (:require
            [clojure.string :as string]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.http-response :as hr]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.web.routes.template :refer :all]))

(defn get-home-page
  "Home page.

  TODO"
  [req]
  (template-page
    req
    "Home"
    [:h1 "TODO"]))
