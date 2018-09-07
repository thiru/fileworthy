(ns fileworthy.web.routes
  "Contains a registry of routes to React components."
  (:require
            [clojure.string :as string]

            [accountant.core :as accountant]
            [bidi.bidi :as bidi]
            [reagent.session :as session]

            [fileworthy.web.pages.about :as about]
            [fileworthy.web.pages.error :as error]
            [fileworthy.web.pages.home :as home]
            [fileworthy.web.pages.layout :as layout]
            [fileworthy.web.pages.login :as login]
            [fileworthy.web.pages.logout :as logout]
            [fileworthy.web.pages.not-found :as not-found]
            [fileworthy.web.state :refer [state]]))

(def app-routes
  ["/" {"" :home-page
        "about" :about-page
        "error" :error-page
        "home" :home-page
        "login" :login-page
        "logout" :logout-page
        "missing-route" :missing-route
        true :not-found-page}])

(swap! state assoc :routes app-routes)

(defmulti page-contents identity)

(defmethod page-contents :about-page [] about/page-ui)
(defmethod page-contents :error-page [] error/page-ui)
(defmethod page-contents :home-page [] home/page-ui)
(defmethod page-contents :login-page [] login/page-ui)
(defmethod page-contents :logout-page [] logout/page-ui)
(defmethod page-contents :not-found-page [] not-found/page-ui)
(defmethod page-contents :default []
  [:div
   [:h1 "Not Found - Missing Route"]
   [:p "It looks like the developer defined a route but didn't fully
        implement it!"]])

(defn page []
  (fn []
    (layout/page-ui page-contents)))

(defn ^:export init! []
  (accountant/configure-navigation!
    {:nav-handler (fn [path]
                    (let [match (bidi/match-route app-routes path)
                          current-page (:handler match)
                          route-params (:route-params match)]
                      (swap! state
                             assoc :current-route
                             {:page current-page
                              :route-params route-params})))
     :path-exists? (fn [path]
                     (boolean (bidi/match-route app-routes path)))})
  (accountant/dispatch-current!))
