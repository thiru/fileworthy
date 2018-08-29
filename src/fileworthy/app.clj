(ns fileworthy.app
  "High-level app details and configuration, used in places like the CLI, web
   server defaults, build definition, etc."
  (:require
            [aero.core :as cfg]
            [figwheel.main.api :as fig]

            [glu.core :refer :all]
            [glu.logging :refer :all]
            [glu.repl :as repl]
            [glu.results :refer :all]

            [fileworthy.web.server :as server]))

(defn start-figwheel
  "Start figwheel in a script-friendly mode."
  []
  ;; Start figwheel build in dev mode:
  (fig/start {:id "dev"
              :options {:main 'fileworthy.main
                        :output-to "html/cljs/main.js"
                        :output-dir "html/cljs/dev"
                        :asset-path "cljs/dev"}
              :config {:css-dirs ["html/css"]
                       :open-url (str "localhost:" (:web-server-port @config))
                       :mode :serve
                       :watch-dirs ["src"]}})
  ;; Start a ClojureScript REPL:
  (fig/cljs-repl "dev"))

(defn start
  "Start the application (nREPL server and web server).

   * `environment`
     * The profile in which to load the config
     * See `aero.core` for more info"

  [& {:keys [environment]}]

  (when (empty? @config)
    (load-config! :profile environment)
    (log :info (fmt "Loaded config: ~A" @config)))
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
