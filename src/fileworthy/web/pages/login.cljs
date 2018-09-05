(ns fileworthy.web.pages.login
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
            [clojure.string :as string]

            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [reagent.core :as r]

            [fileworthy.web.routes :as routes]
            [fileworthy.web.state :refer [state]]))

(defn already-logged-in-ui []
  [:div
    [:h2 (str "Hey " (or (-> @state :user :name) "there") ",")]
    [:p "It looks like you're already logged in."]
    [:p "Would you like to "
      [:a {:href "/logout"} "log out"]
      "?"]])

(defn submit-login
  [form-state]
  (swap! form-state assoc :submitting true)
  (swap! form-state assoc :login-result nil)
  (go (let [creds (select-keys @form-state [:username :password])
            response (<! (http/post "/login" {:edn-params creds}))]
        ;(prn response) ; DEBUG
        (swap! form-state assoc :submitting false)
        (swap! form-state assoc :login-result (:body response))
        (if (:success response)
          (. js/location assign "/"))))) ; Go to the home page (TODO: fix)

(defn login-form-ui []
  (if (empty? (-> @state :page :form))
    (swap! state
           assoc-in [:page :form]
           {:login-result nil
            :password ""
            :submitting false
            :username ""}))
  (let [s (r/cursor state [:page :form])]
    (fn []
      [:div
        [:h1 "Please log in to continue"]
        [:form
          {:on-submit (fn [e]
                        (.preventDefault e)
                        (submit-login s))}
          [:p
            [:input#email.full-width
              {:name "username"
               :on-change #(swap! s assoc :username
                                  (-> % .-target .-value))
               :placeholder "Username or email address"
               :required true
               :title "Username or email address"
               :type "text"
               :value (:username @s)}]]
          [:p
            [:input#password.full-width
              {:name "password"
               :on-change #(swap! s assoc :password
                                  (-> % .-target .-value))
               :placeholder "Password"
               :required true
               :title "Password"
               :type "password"
               :value (:password @s)}]]
          (if (not (empty? (:login-result @s)))
            [:p {:class (-> @s :login-result :level)}
              (-> @s :login-result :message)])
          [:p
            [:button.button.full-width
              {:disabled (:submitting @s)}
              (if (:submitting @s)
                [:span
                  [:i.fas.fa-cog.fa-spin.fa-fw]
                  "Logging in..."]
                "Login")]]]])))

(defn login-page []
  [:div
    {:style {:max-width "30em"
             :margin-left "auto"
             :margin-right "auto"}}
    (if (not (empty? (-> @state :user)))
      [already-logged-in-ui]
      [login-form-ui])])
(routes/add :login-page login-page)
