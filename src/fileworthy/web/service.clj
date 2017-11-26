;; ## Summary
;;
;; Domain-specific web service.
;;
(ns fileworthy.web.service
  (:require [common.utils :refer :all]
            [fileworthy.app :as app]
            [fileworthy.web.utils :refer :all]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]))

;; ## API End-Points

;; TODO

;; ## Routing

(def routes
  "Definition of all HTTP routes."
  #{["/test" :get respond-hello :route-name :test]})

;; ## Service Config

(def service-config
  "A map specifying the web service configuration.

  The default values are configured for production mode.
 
  If `::http/join?` is false, Pedestal will not block the thread that starts
  the web server. This essentially facilitates starting and stopping the server
  inside the process.
 
  Jetty is the underlying web server implementation used. Alternatives include
  Tomcat, immutant, etc."
  {:env :prod
   ;; Allow routes to be reloadable
   ::http/routes #(route/expand-routes (deref #'routes))
   ::http/type   :jetty
   ::http/port   (:port-default app/config)
   ::http/join?  true})
