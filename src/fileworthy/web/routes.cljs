(ns fileworthy.web.routes
  "Client-side routing functionality."
  (:require
            [clojure.string :as string]

            [accountant.core :as accountant]
            [bidi.bidi :as bidi]

            [fileworthy.web.pages.about :as about]
            [fileworthy.web.pages.error :as error]
            [fileworthy.web.pages.home :as home]
            [fileworthy.web.pages.layout :as layout]
            [fileworthy.web.pages.login :as login]
            [fileworthy.web.pages.logout :as logout]
            [fileworthy.web.pages.not-found :as not-found]
            [fileworthy.web.state :refer [state]]
            [fileworthy.web.utils :as utils]))

(def pages
  "We associate the page ids with the function var, rather than the function
   itself because we want to be able to access its metadata. E.g. this is
   where the page's title is stored."
  {:about-page #'about/page-ui
   :error-page #'error/page-ui
   :home-page #'home/page-ui
   :login-page #'login/page-ui
   :logout-page #'logout/page-ui
   :not-found #'not-found/page-ui})

(def routes
  ["/" [["" :home-page]
        ["about" :about-page]
        ["error" :error-page]
        ["home" :home-page]
        ["login" :login-page]
        ["logout" :logout-page]
        [true :not-found-page]]])

(swap! state assoc :routes routes)

(defn ^:export init! []
  (accountant/configure-navigation!
    {:nav-handler (fn [path]
                    (let [match (bidi/match-route routes path)
                          current-page-id (:handler match)
                          current-page-var (or (-> pages current-page-id)
                                               #'not-found/page-ui)
                          route-params (:route-params match)]
                      (swap! state
                             assoc :current-route
                             {:page-id current-page-id
                              :page-var current-page-var
                              :route-params route-params})))
     :path-exists? (fn [path]
                     (boolean (bidi/match-route routes path)))})
  (accountant/dispatch-current!))
