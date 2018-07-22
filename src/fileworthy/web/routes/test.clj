(ns fileworthy.web.routes.test
  "Test page used during development."
  (:require
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [prone.debug :refer [debug]]

            [glu.core :refer :all]
            [glu.logging :refer :all]
            [glu.results :refer :all]

            [fileworthy.web.routes.template :refer :all]))

(defn get-test-root-page
  "General/misc test page."
  [req]
  (template-page
    req
    "Test"
    [:div
      [:h1 "Test Page"]
      (str req)]))

(defn get-test-debug-page
  "Test debugging locals."
  [req]
  (let [x 1
        y 2
        z 3]
    (debug "Testing locals debugging" {:another "map" :to-inspect 123})))
