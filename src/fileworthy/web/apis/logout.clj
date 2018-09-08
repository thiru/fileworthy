(ns fileworthy.web.apis.logout
  "Log out API."
  (:require
            [clojure.string :as str]
            [clojure.pprint :refer :all]

            [ring.util.http-response :as hr]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.core.users :as users]))

(defn get-logout-api
  "Expire user's session."
  [req]
  (if-let [user (users/get-one {:username (-> req :session :username)})]
    (do
      (log :info (str "User '" (:name user) "' logged out"))
      (-> (r :success "You've been successfully logged out")
          hr/ok
          (hr/content-type "application/edn")
          (assoc :session nil)))
    (-> (r :info "You weren't logged in")
        hr/ok
        (hr/content-type "application/edn")
        (assoc :session nil))))
