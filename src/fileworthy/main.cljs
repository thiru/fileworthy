(ns fileworthy.main
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
            [fileworthy.web.state :refer [state]]))

(routes/init!)

(r/render [routes/page]
          (js/document.getElementById "app"))

(utils/load-site-info state)
