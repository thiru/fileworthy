;; ## Summary
;;
;; REPL server.
;;
;; This namespace does not contain any domain-specific code, and so should be
;; easy to use in other projects.
;;
(ns glu.repl
  (:require [clojure.tools.nrepl.server :as nrepl]
            [cider.nrepl :refer (cider-nrepl-handler)]
            [glu.utils :refer :all]
            [glu.reporting :refer :all]
            [glu.logging :refer :all]))

(def instance
  "Contains the nREPL server object and related config parameters.

  It's wrapped in an atom to easily support stopping/restarting.

  * `server`
    * The nREPL server object
  * `config`
    * Configuration parameters for the nREPL server
    * We store these parameters so we know how to restart the server in the
      same way it was originally started"
  (atom {:server nil
         :config {}}))

(defn start
  "Start a network repl for debugging on specified port followed by
  an optional parameters map. The :bind, :transport-fn, :handler,
  :ack-port and :greeting-fn will be forwarded to
  clojure.tools.nrepl.server/start-server as they are."
  [{:keys [port bind transport-fn handler ack-port greeting-fn]
    :or {handler cider-nrepl-handler}}]
  (if (not (nil? (:server @instance)))
    (log :warning
         (str "nREPL server already running on port "
              (-> @instance :server :port)
              ". Start aborted."))
    (try
      (log :debug (str "Starting nREPL server on port " port "..."))
      (reset!
        instance
        {:server
          (nrepl/start-server :port port
                              :bind bind
                              :transport-fn transport-fn
                              :handler handler
                              :ack-port ack-port
                              :greeting-fn greeting-fn)
          :config {:port port
                   :bind bind
                   :transport-fn transport-fn
                   :handler handler
                   :ack-port ack-port
                   :greeting-fn greeting-fn}})
      (log :success (str "nREPL server started on port " port))
      (catch Throwable t
        (log :error (str "Failed to start nREPL server on port " port))
        (throw t)))))

(defn stop
  "Stop the running nREPL server (if any)."
  []
  (if (nil? (:server @instance))
    (log :warning "nREPL server doesn't appear to be running")
    (do
      (log :debug
           (str "Stopping nREPL server on port "
                (-> @instance :config :port)
                "..."))
      (nrepl/stop-server (:server @instance))
      (swap! instance assoc :server nil)
      (log :info
           (str "nREPL server on port "
                (-> @instance :config :port)
                " stopped")))))

(defn restart
  "Restart the running nREPL server (if any)."
  []
  (stop)
  (start (:config @instance)))
