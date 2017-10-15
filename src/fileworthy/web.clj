(ns fileworthy.web
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]))

(defn respond-hello [request]
  (let [nm (get-in request [:query-params :name])]
    {:status 200 :body (str "Hello there " nm "\n")}))

(def routes
  (route/expand-routes
    #{["/greet" :get respond-hello :route-name :greet]}))

(defn create-server []
  (http/create-server
    {::http/routes routes
     ::http/type   :jetty
     ::http/port   8080}))

(defn start []
  (http/start (create-server)))
