(ns fileworthy.web.routes.loginout
  "Log in/out pages and APIs."
  (:require
            [clojure.string :as str]
            [clojure.pprint :refer :all]

            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.http-response :as hr]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.core.users :as users]
            [fileworthy.web.routes.template :refer :all]))

(defn post-login-api
  "Handle user login attempt."
  [req]
  (let [username (-> req :params :username)
        password (-> req :params :password)
        user (users/get-one {:username username})
        valid? (and (non-empty? user)
                    (= password (:password user)))]
    (if valid?
      (do
        (log :info (str "User '" (:name user) "' successfully logged in"))
        (-> (r :success "Login successful!"
               :user (select-keys user [:name :roles :username]))
            hr/ok
            (hr/content-type "application/edn")
            (assoc :session {:username (-> user :username)})))
      (do
        (log :warning (str "User '" username "' failed login"))
        ;; Slow down hack attempts :/
        (Thread/sleep 1500)
        (-> (r :error "Invalid username or password")
            hr/unauthorized
            (hr/content-type "application/edn"))))))

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
