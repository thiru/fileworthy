;; ## Summary
;;
;; Common web-specific utilties.
;;
;; This namespace does not contain any domain-specific code.
;;
(ns fileworthy.web.utils
 (:require [io.pedestal.http :as http]
           [io.pedestal.http.route :as route]
           [io.pedestal.http.route.definition.table :refer [table-routes]]
           [io.pedestal.test :refer [response-for]]))

;; ## HTTP Responses

(def content-types
  "A map of names of common mime content types."
  {:html "text/html"
   :json "application/json"
   :text "text/plain"
   :xml "text/xml"})

(defn response
  "A map describing a generic HTTP response with status, body and headers.
  
  * `status`
    * An integer HTTP status code
  * `body`
    * A string containing an HTTP response body
  * `headers`
    * An optional list of headers
    * Values should be given in pairs
    * E.g. `(response 200 \"BODY\" \"Content-Type\" \"text/html\")`"
  [status body & {:as headers}]
  {:status status :body body :headers headers})

;; Some helper functions for common HTTP responses.

(def http-ok (partial response 200))
(def http-created (partial response 201))
(def http-bad-request (partial response 400))
(def http-unauthorised (partial response 401))
(def http-forbidden (partial response 403))
(def http-not-found (partial response 404))
(def http-server-error (partial response 500))

(defn respond-hello
 "Respond with a simple hello message, including an optional name if
  specified as a query parameter named `name`.
  
  * `request`
    * A map containing a (Pedestal) HTTP request"
 [request]
 (let [nm (get-in request [:query-params :name])]
   (http-ok (str "Hello " nm "\n"))))

;; ## Testing/Inspection

(defn print-routes
 "Print all HTTP routes.
 
 * `routes`
   * A vector of all definied routes"
 [routes]
 (route/print-routes (table-routes routes)))

(defn test-route-response
  "Get the response of a route without performing any actual HTTP requests.
  
  * `service`
    * A Pedestal web service
  * `verb`
    * A keyword specifying an HTTP verb
    * E.g. get, post, put, delete
  * `url`
    * The URL to test"
  [service verb url]
  (response-for (::http/service-fn service) verb url))

(defn verify-route
  "Verify that the given verb and URL are recognised by the router.

  * `routes`
   * A vector of all defined routes
  * `verb`
    * A keyword specifying an HTTP verb
    * E.g. get, post, put, delete
  * `url`
    * The URL to test"
  [routes verb url]
  (route/try-routing-for (table-routes routes) :prefix-tree url verb))

(defn url-for-route
  "Get the URL of the named route.
  
  * `routes`
   * A vector of all defined routes
  * `route-name`
    * A keyword specifying a route name
  * `opts`
    * Options passed along to `route/url-for-routes`
    * See [API docs](http://pedestal.io/api/pedestal.route/io.pedestal.http.route.html#var-url-for-routes)
    * E.g. `(url-for-route ::test :absolute? true)`"
  [routes route-name & opts]
  (let [f (route/url-for-routes (table-routes routes))
        defaults   {:host "localhost" :scheme :http :port 8080}
        route-opts (flatten (seq (merge defaults (apply hash-map opts))))]
    (apply f route-name route-opts)))
