(ns fileworthy.web.state
  (:require
            [clojure.string :as string]

            [reagent.core :as r]

            [fileworthy.web.utils :as utils]))

(def default-state
  {:settings-nav {:visible? false}})

(defn get-state []
  (merge (utils/parse-site-info) default-state))

(defonce
  ^{:doc "Global state info for the page/website."}
  state
  (r/atom (get-state)))

(defn reset-state! []
  (reset! state (get-state)))
