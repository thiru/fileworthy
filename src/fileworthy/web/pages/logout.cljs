(ns fileworthy.web.pages.logout
  (:require
            [clojure.string :as string]

            [fileworthy.web.routes :as routes]))

(defn logout-page []
  [:div
    [:h2 "You were successfully logged out"]
    [:a.button {:href "/login"} "Log in again"]])
(routes/add :logout-page logout-page)
