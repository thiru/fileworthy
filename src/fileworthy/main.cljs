(ns fileworthy.main
  (:require
            [clojure.string :as string]

            [cljs.pprint :refer [pp pprint]]
            [cljs.repl :refer [doc]]

            [reagent.core :as r]

            [fileworthy.web.utils :as utils]
            [fileworthy.web.routes :as routes]
            [fileworthy.web.pages.layout :refer [layout-ui]]
            [fileworthy.web.pages.about :as about-page]
            [fileworthy.web.pages.home :as home-page]
            [fileworthy.web.pages.login :as login-page]
            [fileworthy.web.pages.logout :as logout-page]
            [fileworthy.web.pages.not-found :as not-found-page]
            [fileworthy.web.state :refer [state reset-state!]]))

(r/render [layout-ui] (js/document.getElementById "app"))
