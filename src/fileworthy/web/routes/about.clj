;; ## Summary
;;
;; The about page.
;;
(ns fileworthy.web.routes.about
  (:require
            [clojure.string :as string]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [java-time :as jt]

            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [fileworthy.app :as app]
            [fileworthy.web.routes.template :refer :all]))

(defn get-about-page
  "About page."
  [req]
  (template-page
    req
    "About"
    [:div
      [:p "Fileworthy is a simple website to manage your notes and files "
          "across all your devices."]

      [:table.brief-table
        [:tr
          [:td "Version"]
          [:td (:version @app/config)]]
        [:tr
          [:td "Last Updated"]
          [:td.utc-time (:updated @app/config)]]
        [:tr
          [:td "Source Code"]
          [:td [:a {:href "https://github.com/thiru/fileworthy"}
                 "Hosted on Github"]]]
        [:tr
          [:td "License"]
          [:td
            [:a {:href "https://www.gnu.org/licenses/gpl-3.0.html"}
              "GPLv3"]]]
        [:tr
          [:td "Copyright"]
          [:td "2016-"
               (.toString (jt/year (jt/local-date)))
               " Thirushanth Thirunavukarasu"]]]]))
