(ns fileworthy.web.state
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
            [clojure.string :as string]
            [cljs.core.async :refer [<!]]

            [cljs-http.client :as http]
            [reagent.core :as r]))

(def default-state
  {:settings-nav {:visible? false}})

(defonce
  ^{:doc "Global state info for the page/website."}
  state
  (r/atom default-state))

(defn load-site-info!
  "Get EDN containing general info about the page/website and merge it into
   `state`.

   This includes things like the site configuration, the currently logged in
   user, etc."
  []
  (go (let [response (<! (http/get "/api/site-info"))]
        (prn response)
        (swap! state merge @state (:body response)))))

(defn init! []
  (load-site-info!))
