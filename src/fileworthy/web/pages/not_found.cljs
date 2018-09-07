(ns fileworthy.web.pages.not-found
  (:require
            [clojure.string :as string]))

(defn page-ui []
  [:div
    [:h1 "Not Found"]
    [:p "Sorry, the requested page or resource was not found."]
    [:a.button {:href "/"} "Go back to the home page"]])
