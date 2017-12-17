;; ## Summary
;;
;; High-level app details and configuration, used in places like the CLI, web
;; server defaults, build definition, etc.
;;
(ns fileworthy.app
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thiru.utils :refer :all]
            [thiru.config :as cfg]
            [thiru.debugnlog :refer :all]
            [thiru.reporting :refer :all]))

(def info
  "General app info.
  
  These properties are not meant to be modified, unlike `config`."
  {:name "Fileworthy"
   :version "0.22.0"
   :description (str "Fileworthy is a simple website to manage your notes "
                     "and files across all your devices.")})

(def config-defaults
  "Default configuration values.
 
  This will be used if a user supplied config doesn't exist or can't be found."
  {:log-level :debug
   :port 8023})

(def config
  "Contains the global app configuration map.
  
  We store it in an atom so it can be live-reloaded."
  (atom
    (let [c (cfg/load-config "config.edn" config-defaults)]
      (log :info (str "App config loaded: " c))
      c)))

(defn reload-config
  "Reload config from one of the predefined user directories.
  
  * `file-name`
    * If specified, it is will be used as the config file name
    * Otherwise 'config.edn' is used"
  ([] (reload-config "config.edn"))
  ([file-name]
   (reset! config (cfg/load-config file-name config-defaults))))
