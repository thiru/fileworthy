(ns fileworthy.web.routes.login
  "Log in API."
  (:require
            [clojure.string :as str]
            [clojure.pprint :refer :all]

            [ring.util.http-response :as hr]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.core.users :as users]))

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
