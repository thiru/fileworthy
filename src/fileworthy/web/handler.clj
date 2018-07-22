(ns fileworthy.web.handler
  "Web server route handling."
  (:require
            [clojure.string :as string]

            [compojure.core :refer [context defroutes GET POST]]
            [compojure.route :as route]
            [prone.middleware :as prone]
            [ring.handler.dump :refer [template]]
            [ring.middleware.defaults :refer :all]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace-log]]
            [ring.util.http-response :as hr]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.web.routes.about :refer :all]
            [fileworthy.web.routes.error-404 :refer :all]
            [fileworthy.web.routes.error-500 :refer :all]
            [fileworthy.web.routes.home :refer :all]
            [fileworthy.web.routes.loginout :refer :all]
            [fileworthy.web.routes.test :refer :all]))

(defroutes all-routes
  (GET "/" req (get-home-page req))
  (GET "/about" req (get-about-page req))
  (GET "/login" req (get-login-page req))
  (POST "/login" req (post-login-page req))
  (GET "/logout" req (get-logout-page req))
  (context "/test" req
    (GET "/" [] (get-test-root-page req))
    (GET "/req-map" [] template)
    (GET "/debug" req (get-test-debug-page req))
    (GET "/500" []
         (throw (Exception. "This is an intentional error test page."))))
  (route/not-found get-error-404-page))

(defn authorized?
  "Determine whether the current request is authenticated.

  Basically, all pages require authentication except the about page, log in/out
  pages, and static content."
  [req]
  (or (not (string/blank? (-> req :session :username)))
      (re-matches
        #"^/(about|css/|deps/|images/|js/|login|logout|manifest.json)$"
        (-> req :uri))))

(defn wrap-auth
  "Authentication middleware."
  [handler]
  (fn [req]
    (if (authorized? req)
      (handler req)
      (hr/temporary-redirect
        (str
          "/login"
          (if-not (contains? #{"/" "/login"} (-> req :uri))
            (str "?go-back-to=" (-> req :uri))))))))

(defn get-handler
  "Get handler appropriate for development or production environment.

  Note that public HTML resources are served from a local relative directory to
  the application itself. I.e. the resources are not served from within the jar
  file, though they may be there as well."
  [dev?]
  (cond-> (if dev? #'all-routes all-routes)
    dev? wrap-reload
    true wrap-auth
    true (wrap-defaults (-> site-defaults
                            ;; TODO: review anti-forgery use in dev vs prod
                            (assoc-in [:security :anti-forgery] false)
                            (update-in [:static] dissoc :resources)
                            (assoc-in [:static :files] "html")))
    true (wrap-stacktrace-log {:color? true})
    dev? (prone/wrap-exceptions
           {:app-namespaces ["fileworthy" "glu"]
            :print-stacktraces? false})
    (not dev?) (wrap-exception-friendly)))
