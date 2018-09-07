(ns fileworthy.web.pages.error
  (:require
            [clojure.string :as string]

            [bidi.bidi :as bidi]

            [fileworthy.web.state :refer [state]]))

(defn page-ui []
  [:div
    [:h1 "Error"]
    [:p "Sorry, an unexpected error occcurred."]
    [:p "If this keeps happening please report it to the website owner."]
    [:a.button {:href "/"} "Go back to the home page"]])
