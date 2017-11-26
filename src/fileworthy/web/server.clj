;; ## Summary
;;
;; General and high-level web server functionality, mostly around web server
;; life-cycle management.
;; 
;; This namespace does not contain any domain-specific code.
;;
(ns fileworthy.web.server
  (:require [common.utils :refer :all]
            [fileworthy.app :as app]
            [fileworthy.web.service :as service]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]))

;; `runnable-service` will hold an instance of the web server.
;;
;; It is stored in a global variable so we can easily stop and restart it.
(defonce runnable-service (atom nil))

(defn start
  "Start a new web server at the specified port."
  ([] (start (:port-default app/config)))
  ([port]
   (log :info (str "Starting web server on port " port))
   (reset! runnable-service
     (http/start
       (http/create-server
         (assoc service/service-config ::http/port port))))))

(defn start-dev
  "Start a new web server at the specified port, in development mode.
  
  Development mode facilitates

  * Restarting the service
  * Less strict security protocols"
  ([] (start-dev (:port-default app/config)))
  ([port]
   (log :info (str "Starting web server on port " port " (DEV MODE)"))
   (reset! runnable-service
    (-> service/service-config
        (merge {:env :dev
                ::http/port port
                ::http/join? false
                ;; All origins are allowed in dev mode
                ::http/allowed-origins {:creds true
                                        :allowed-origins (constantly true)}
                ;; Content Security Policy (CSP) is mostly turned off in dev
                ;; mode
                ::http/secure-headers {:content-security-policy-settings
                                        {:object-src "none"}}})
         ;; Wire up interceptor chains
        http/default-interceptors
        http/dev-interceptors
        http/create-server
        http/start))))

(defn stop
  "Stop the running web server."
  []
  (log :info "Stopping web server on port " (::http/port @runnable-service))
  (http/stop @runnable-service))

(defn restart
  "Restart the running web server."
  []
  (stop)
  (start-dev))
