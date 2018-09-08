(ns fileworthy.web.handler
  "Web server route handling."
  (:require
            [clojure.string :as string]

            [compojure.core :refer [context defroutes ANY GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer :all]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace-log]]
            [ring.util.http-response :as hr]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.web.routes.site-info :refer :all]
            [fileworthy.web.routes.login :refer :all]
            [fileworthy.web.routes.logout :refer :all]))

(defroutes all-routes
  (context "/api" req
    (GET  "/site-info" req (get-site-info-api req))
    (POST "/login" req (post-login-api req))
    (GET  "/logout" req (get-logout-api req))
    ;; Unlike with non-API GET requests we'll return a 404 if the route isn't
    ;; defined
    (ANY  "*" req (-> "API not found"
                      hr/not-found
                      (hr/content-type "text/plain"))))
  ;; If the (non-API) GET request isn't defined return a 200 response with the
  ;; base page. The actual routing/rendering happens on the client-side
  ;; afterall since this is a SPA.
  (GET "*" req (-> (slurp "html/home.html")
                   hr/ok
                   (hr/content-type "text/html")))
  ;; Non-GET requests should response with a client error. These may also by
  ;; potentially malicious.
  (ANY "*" req (-> "Invalid request"
                   hr/bad-request
                   (hr/content-type "text/plain"))))

(defn get-handler
  "Get handler appropriate for development or production environment.

  Note that public HTML resources are served from a local relative directory to
  the application itself. I.e. the resources are not served from within the jar
  file, though they may be there as well."
  [dev?]
  (cond-> (if dev? #'all-routes all-routes)
    dev? wrap-reload
    true (wrap-defaults (-> site-defaults
                            ;; TODO: review anti-forgery use in dev vs prod
                            (assoc-in [:security :anti-forgery] false)
                            (update-in [:static] dissoc :resources)
                            (assoc-in [:static :files] "html")))
    true (wrap-restful-format :formats [:edn :json])
    true (wrap-stacktrace-log {:color? true})))
