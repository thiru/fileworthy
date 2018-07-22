(ns glu.repl
  "Utils around managing an nREPL server."
  (:require
            [clojure.java.io :as io]
            [clojure.tools.nrepl.server :as nrepl]

            [cider.nrepl :refer [cider-nrepl-handler]]

            [glu.core :refer :all]
            [glu.results :refer :all]
            [glu.logging :refer :all]))

(def nrepl-port-file
  "This file is used by tools (like Vim Fireplace) to know what port to connect
   to the nREPL server."
  ".nrepl-port")

(def instance
  "Contains the nREPL server object and related config parameters.

  * `server`
    * The nREPL server object
  * `config`
    * Configuration parameters for the nREPL server
    * We store these parameters here so we know how to restart the server in
      the same way it was originally started"
  (atom {:server nil
         :config {}}))

(defn start!
  "Starts a network REPL.

   * `port`
     * The port of the REPL server
   * The remaining optional parameters will be passed along to
     `clojure.tools.nrepl.server/start-server` as is

   Side-effects:
   * Updates `instance`

   Returns the nREPL server instance."
  [port & {:keys [bind transport-fn handler ack-port greeting-fn]
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
        {:server (nrepl/start-server :port port
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
      (spit nrepl-port-file port)
      (log :success (str "nREPL server started on port " port))
      @instance
      (catch Throwable t
        (log :error (str "Failed to start nREPL server on port " port))
        (throw t)))))

(defn stop!
  "Stop the running nREPL server (if any).

   Side-effects:
   * Updates `instance`

   Returns `nil`."
  []
  (if (nil? (:server @instance))
    (log :warning "nREPL server doesn't appear to be running")
    (do
      (log :debug
           (str "Stopping nREPL server on port "
                (-> @instance :config :port)
                "..."))
      (if (file-exists? nrepl-port-file)
        (io/delete-file nrepl-port-file true))
      (nrepl/stop-server (:server @instance))
      (swap! instance assoc :server nil)
      (log :info
           (str "nREPL server on port "
                (-> @instance :config :port)
                " stopped")))))

(defn restart!
  "Restart the running nREPL server (if any).

   Side-effects:
   * Updates `instance`"
  []
  (stop!)
  (start! (-> @instance :config :port)))
