(ns fileworthy.web.utils
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
            [clojure.string :as string]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cljs.reader :as reader]))

(defn load-site-info
  "Get EDN containing general info about the page/website.

   This includes things like the site configuration, the currently logged in
   user, etc.
   * `state`
     * The EDN is merged into this atom"
  [state]
  (go (let [response (<! (http/get "/api/site-info"))]
        (prn response)
        (swap! state merge @state (:body response)))))
