(ns fileworthy.web.routes
  "Contains a registry of routes to React components."
  (:require
            [clojure.string :as string]))

(defonce
  ^{:doc "Contains a mapping of page ids to React components."}
  all (atom {:not-found-page (fn [] [:p "Page not found."])}))

(defn get-component
  [page-id]
  (get @all page-id (:not-found-page @all)))

(defn add
  [page-id component]
  (swap! all assoc page-id component))
