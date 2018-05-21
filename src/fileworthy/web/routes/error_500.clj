;; ## Summary
;;
;; The "internal server error" (500) page.
;;
(ns fileworthy.web.routes.error-500
  (:require
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]

            [glu.logging :refer :all]
            [glu.reporting :refer :all]
            [glu.utils :refer :all]

            [fileworthy.app :as app]
            [fileworthy.web.routes.template :refer :all]))

(defn get-error-500-page
  "Internal server error page (HTTP status code 500)."
  [req]
  (template-page
    req
    "Error"
    [:div
      [:h1 "Error"]
      [:p "Sorry, an unexpected error occurred on the server."]
      [:p (str "Please try again later, or if the problem persists "
               "contact the website administrator.")]
      [:a.button {:href "/"} "Go back to the home page"]]))

(defn wrap-exception-friendly
  "Middleware that shows a friendly error page for server/backend exceptions."
  [handler]
  (fn [request]
    (try (handler request)
      (catch Exception e
         {:status 500
          :body (get-error-500-page request)}))))
