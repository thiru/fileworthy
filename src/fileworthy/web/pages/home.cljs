(ns fileworthy.web.pages.home
  (:require
            [clojure.string :as string]

            [fileworthy.web.state :refer [state]]))

(defn page-ui []
  [:h1 "Welcome to " (-> @state :site-info :site-name)])
