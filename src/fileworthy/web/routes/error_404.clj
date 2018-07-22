(ns fileworthy.web.routes.error-404
  (:require
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.web.routes.template :refer :all]))

(defn get-error-404-page
  "'Not found' page (HTTP status code 404)."
  [req]
  (template-page
    req
    "Not Found"
    [:div
      [:h1 "Not Found"]
      [:p "Sorry, the requested page or resource was not found."]
      [:a.button {:href "/"} "Go back to the home page"]]))

