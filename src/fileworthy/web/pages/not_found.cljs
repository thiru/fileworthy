(ns fileworthy.web.pages.not-found
  (:require
            [clojure.string :as string]

            [fileworthy.web.routes :as routes]))

(defn not-found-page []
  [:div
    [:h1 "Not Found"]
    [:p "Sorry, the requested page or resource was not found."]
    [:a.button {:href "/"} "Go back to the home page"]])

(routes/add :not-found-page not-found-page)
