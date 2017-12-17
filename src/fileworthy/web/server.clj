;; ## Summary
;;
;; General and high-level web server functionality, mostly around web server
;; life-cycle management.
;; 
(ns fileworthy.web.server
  (:require [thiru.utils :refer :all]
            [yada.yada :refer [listener resource as-resource]]))

;; This will hold an instance of the web server.
;;
;; It is stored in a global variable so we can easily stop and restart it.
(defonce server (atom nil))

(defn start
  "Start a new web server at the specified port."
  [port]
  (log :info (str "Starting web server on port " port))
  (reset! server
          (listener
           ["/"
            [
             ["hello" (as-resource "Hello world!")]
             ["test" (resource {:produces "text/plain"
                                :response "This is a test!"})]
             ;; This defines the 404 response
             [true (as-resource nil)]]]
           {:port port})))

(defn stop
  "Stop the running web server."
  []
  (log :info "Stopping web server on port TODO")
  ((:close @server)))

(defn restart
  "Restart the running web server."
  []
  (stop)
  (start 8023))
