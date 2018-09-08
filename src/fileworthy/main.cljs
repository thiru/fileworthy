(ns ^:figwheel-hooks fileworthy.main
  (:require
            [clojure.string :as string]

            [cljs.pprint :refer [pp pprint]]
            [cljs.repl :refer [doc]]

            [accountant.core :as accountant]
            [bidi.bidi :as bidi]
            [reagent.core :as r]
            [reagent.session :as session]

            [fileworthy.web.utils :as utils]
            [fileworthy.web.routes :as routes]
            [fileworthy.web.pages.about :as about-page]
            [fileworthy.web.pages.home :as home-page]
            [fileworthy.web.pages.login :as login-page]
            [fileworthy.web.pages.logout :as logout-page]
            [fileworthy.web.pages.not-found :as not-found-page]
            [fileworthy.web.state :as state]))

(defn ^:export mount []
  (r/render [routes/page]
            (js/document.getElementById "app")))

(defn ^:after-load re-render []
  (mount))

(defonce init!
  (do
    (state/init!)
    (routes/init!)
    (mount)))
