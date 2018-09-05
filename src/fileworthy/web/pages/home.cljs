(ns fileworthy.web.pages.home
  (:require
            [clojure.string :as string]

            [fileworthy.web.routes :as routes]
            [fileworthy.web.state :refer [state]]))

(defn home-page []
  [:h1 "Welcome to " (-> @state :site-info :site-name)])
(routes/add :home-page home-page)
