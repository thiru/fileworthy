(ns fileworthy.web.pages.logout
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
            [clojure.string :as string]

            [accountant.core :as accountant]
            [bidi.bidi :as bidi]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]

            [fileworthy.web.state :refer [state]]))

(defn do-logout []
  (go (let [response (<! (http/get "/logout"))]
        (if (:success response)
          (do
            (swap! state assoc :user nil)
            (accountant/navigate! "/logout"))
          (accountant/navigate! "/error?logout-failed")))))

(defn page-ui []
  [:div
   [:h2 "You were successfully logged out"]
   [:a.button {:href (bidi/path-for (:routes @state) :login-page)}
    "Log in again"]])
