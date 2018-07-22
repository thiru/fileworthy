(ns fileworthy.app
  "High-level app details and configuration, used in places like the CLI, web
   server defaults, build definition, etc."
  (:require
            [aero.core :as cfg]

            [glu.core :refer :all]
            [glu.fsreload :as fsreload]
            [glu.logging :refer :all]
            [glu.repl :as repl]
            [glu.results :refer :all]

            [fileworthy.web.server :as server]))

(defonce
  ^{:doc "Track whether we started the file-system watcher (code changes)."}
  fs-watch-started?
  (atom nil))

(defn start
  "Start the application (nREPL server and web server).

   * `environment`
     * The profile in which to load the config
     * See `aero.core` for more info"

  [& {:keys [environment]}]

  (when (empty? @config)
    (load-config! :profile environment)
    (log :info (fmt "Loaded config: ~A" @config)))
  (when (not @fs-watch-started?)
    (fsreload/start-watch!)
    (reset! fs-watch-started? true))
  (repl/start! (:nrepl-port @config))
  (server/start! false (:web-server-port @config)))

(defn stop
  "Stop the application (nREPL server and web server)."
  []
  (server/stop!)
  (repl/stop!))

(defn restart
  "Restart the application (nREPL server and web server)."
  []
  (server/restart!)
  (repl/restart!))
