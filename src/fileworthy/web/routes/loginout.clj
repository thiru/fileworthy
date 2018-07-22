(ns fileworthy.web.routes.loginout
  "Log in/out pages and APIs."
  (:require
            [clojure.string :as str]
            [clojure.pprint :refer :all]

            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [prone.debug :refer [debug]]
            [ring.util.http-response :as hr]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]

            [fileworthy.core.users :as users]
            [fileworthy.web.routes.template :refer :all]))

(defn get-login-page
  "The login page."
  [req & {:keys [failed-attempt? username password]}]
  (let [user (users/get-one {:username (-> req :session :username)})]
    (template-page
      req
      "Login"
      (if (non-empty? user)
        [:div
          [:h2 (str "Hey " (:name user) ",")]
          [:p "It looks like you're already logged in."]
          [:p "Would you like to "
            [:a {:href "/logout"} "log out"]
            "?"]]
        [:div
          [:h1 "Please log in to continue"]
          [:form {:action (if (-> req :params :go-back-to)
                            (str "/login?go-back-to="
                                 (-> req :params :go-back-to))
                            "/login")
                  :method "post"}
            [:p
              [:input#email.full-width
                {:name "username"
                 :placeholder "Username or email address"
                 :title "Username or email address"
                 :type "text"
                 :value username}]]
            [:p
              [:input#password.full-width
                {:name "password"
                 :placeholder "Password"
                 :title "Password"
                 :type "password"
                 :value password}]]
            (if failed-attempt?
              [:p.error "Invalid username or password"])
            [:p
              [:button.button.full-width "Login"]]]])
      :user user
      :script-files ["pages/loginout.js"]
      :css-files ["pages/loginout.css"])))

(defn post-login-page
  "Handle user login attempt."
  [req]
  (let [go-back-to-url (-> req :params :go-back-to)
        username (get-in req [:form-params "username"])
        password (get-in req [:form-params "password"])
        user (users/get-one {:username username})
        valid? (and (non-empty? user)
                    (= password (:password user)))]
    (if valid?
      (do
        (log :info (str "User '" (:name user) "' successfully logged in"))
        (-> (template-page
              req
              "Login"
              [:div#login-successful {:data-go-back-to-url go-back-to-url}
                [:h1 (str "Welcome " (:name user) "!")]
                [:p
                 [:i.fas.fa-cog.fa-spin]
                 " We're logging you in now..."]]
              :user user
              :script-files ["pages/loginout.js"]
              :css-files ["pages/loginout.css"])
            (hr/ok)
            (hr/content-type "text/html")
            (assoc :session {:username (-> user :username)})))
      (do
        (log :warning (str "User '" username "' failed login"))
        (-> (get-login-page
              req
              :failed-attempt? true
              :username username
              :password password)
            (hr/unauthorized)
            (hr/content-type "text/html"))))))

(defn get-logout-page
  [req]
  (if-let [user (users/get-one {:username (-> req :session :username)})]
    (log :info (str "User '" (:name user) "' logged out")))
  (-> (template-page
        ;; Pre-emptively nullify the session so the template page doesn't show
        ;; any user-specific elements. This is necessary even though we nullify
        ;; the session further down this threading macro, which is done too
        ;; late for the template page to see it.
        (assoc req :session nil)
        "Logged Out"
        [:div
          [:h2 "You were successfully logged out"]
          [:a.button {:href "/"} "Go back to the home page"]])
      (hr/ok)
      (hr/content-type "text/html")
      (assoc :session nil)))
