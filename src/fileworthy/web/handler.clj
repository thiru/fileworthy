(ns fileworthy.web.handler
  "Web server route handling."
  (:require
            [clojure.string :as string]

            [compojure.core :refer [context defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer :all]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace-log]]
            [ring.util.http-response :as hr]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.web.routes.template :refer [template-page]]
            [fileworthy.web.routes.loginout :refer :all]))

(defroutes all-routes
  (POST "/login" req (post-login-api req))
  (GET "/logout" req (get-logout-api req))
  (GET "*" req (template-page req)))

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
