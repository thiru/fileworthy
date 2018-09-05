(ns fileworthy.web.pages.about
  (:require
            [clojure.string :as string]

            [fileworthy.web.routes :as routes]
            [fileworthy.web.state :refer [state]]))

(defn about-page []
  [:div
    [:p "Fileworthy is a simple website to manage your notes and files "
        "across all your devices."]

    [:table.brief-table
      [:tbody
        [:tr
          [:td [:b "Version"]]
          [:td (-> @state :site-info :version)]]
        (let [updated-str (-> @state :site-info :updated)
              abs-time (.format (js/moment updated-str)
                                "dddd, MMMM Do YYYY, h:mm:ss a")
              rel-time (.fromNow (js/moment updated-str))]
          [:tr
            [:td [:b "Last Updated"]]
            [:td {:title abs-time} rel-time]])
        [:tr
          [:td [:b "Source Code"]]
          [:td [:a {:href "https://github.com/thiru/fileworthy"}
                 "Hosted on Github"]]]
        [:tr
          [:td [:b "License"]]
          [:td
            [:a {:href "https://www.gnu.org/licenses/gpl-3.0.html"}
              "GPLv3"]]]
        [:tr
          [:td [:b "Copyright"]]
          [:td "2016-"
               (.getFullYear (js/Date.))
               " Thirushanth Thirunavukarasu"]]]]])
(routes/add :about-page about-page)
