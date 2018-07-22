(ns fileworthy.web.routes.error-403
  (:require
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.web.routes.template :refer :all]))

(defn get-error-403-page
  "'Not found' page (HTTP status code 403)."
  [req]
  (template-page
    req
    "Not Authorized"
    [:div
      [:h1 "Not Authorized"]
      [:p "Sorry, you don't have access to this page or resource."
          "If this is an error, please contact your administrator."]
      [:a.button {:href "/"} "Go back to the home page"]]))

