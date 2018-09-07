(ns fileworthy.web.state
  (:require
            [clojure.string :as string]

            [reagent.core :as r]

            [fileworthy.web.utils :as utils]))

(def default-state
  {:settings-nav {:visible? false}})

(defonce
  ^{:doc "Global state info for the page/website."}
  state
  (r/atom default-state))
