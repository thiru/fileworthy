(ns fileworthy.web.server
  "General and high-level web server functionality, mostly around web server
   life-cycle management."
  (:require
            [clojure.java.io :as io]

            [ring.adapter.jetty :refer [run-jetty]]

            [glu.logging :refer :all]
            [glu.core :refer :all]

            [fileworthy.web.handler :as handler]))

;;; Contains the web server instance, the main handler (`site`) and other
;;; properties to support restarting."
;;;
(defonce instance
  (atom {:dev? true
         :site nil
         :port (:web-server-port @config)
         :server nil}))

(defn start-web-server!
  "Start the web server.

  This updates the `instance` atom.

  * `dev?`
    * Whether to run the server in development mode
  * `port`
    * The web server port
  * `handler`
    * The handler function containing the site routes"
  [dev? port handler]
  (reset! instance
          {:dev? dev?
           :site handler
           :port port
           :server (run-jetty handler {:port port :join? false})}))

(defn start!
  "Start web server.

  If a web server is already the function is aborted and an error is logged.

  * `dev?`
    * Whether to run the server in development mode
    * If true, `wrap-reload` is used to dynamically reload source code when
      routes, etc. change
  * `port`
    * The port of the web server
    * The default is to use the value specified in the config file"
  ([] (start! true (:web-server-port @config)))
  ([dev? port]
   (if (not (nil? (:server @instance)))
     (log :warning
          (str "Web server already running on port "
               (:port @instance)
               " in "
               (if (:dev? @instance) "development" "production")
               " mode. Start aborted."))
     (do
       (log :debug
            (str "Starting web server on port " port " in "
                 (if dev? "development" "production")
                 " mode..."))
       (start-web-server! dev? port (handler/get-handler dev?))
       (log :success
            (str "Web server started on port " port " in "
                 (if dev? "development " "production ") "mode"))))))

(defn stop!
  "Stop the running web server (if any)."
  []
  (if (nil? (:server @instance))
    (log :warning "Web server doesn't appear to be running")
    (do
      (log :debug "Stopping web server...")
      (.stop (:server @instance))
      (swap! instance assoc :server nil)
      (log :info "Web server stopped"))))

(defn restart!
  "Restart the running web server (if any)."
  []
  (stop!)
  (start! (:dev? @instance) (:port @instance)))
