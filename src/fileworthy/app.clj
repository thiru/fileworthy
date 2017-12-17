;; ## Summary
;;
;; High-level app details and configuration, used in places like the CLI, web
;; server defaults, build definition, etc.
;;
(ns fileworthy.app
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thiru.utils :refer :all]
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

(defn load-config-from
  "Load an EDN config file from the given path and return it's contents.
  
  If the config file doesn't exist at the given path `nil` is returned."
  [path]
  (log :debug (str "Attempting to load config at '" path "'..."))
  (let [file (io/as-file path)
        file-exists (.exists file)
        config (if file-exists
                 (or (-> path slurp edn/read-string)
                     ""))]
    (if file-exists
      (log :success
           (str "Config found at '" path "'"
                (if (empty? config) ", though it's empty" "")))
      (log :debug (str "Config not found at '" path "'")))
    config))

(defn load-config
  "Attempt to load an EDN config file with the given name from the following
  locations (in order):
  
  * Current working directory
  * User's config directory
    * Using the XDG_CONFIG_HOME environment variable
    * E.g. *~/.config/fileworthy*
  * User's home directory (\\*nix)
    * Using the HOME environment variable
    * This may also exist in Windows, e.g. Git Bash or WSL
    * E.g. *~/fileworthy*
  * User's home directory on Windows
    * Using the USERPROFILE environment variable
    * E.g. *C:\\Users\\Me\\fileworthy*"
  [file-name]
  (merge
    config-defaults
    ;; First, try loading config from the current dir
    (as-> (load-config-from file-name) config

       ;; If that fails we search through the following environment variables.
       ;; Note that the order of the environment variables specified here is
       ;; significant, as they will be searched in this order.
       (if (nil? config)
         (let [env-vars ["XDG_CONFIG_HOME" "HOME" "USERPROFILE"]]
           (loop [i 0]
             (let [env-var (nth env-vars i)
                   env-var-path (System/getenv env-var)
                   file-path (if (non-empty? env-var-path)
                               (io/file env-var-path "fileworthy" file-name))
                   config (if file-path
                            (load-config-from file-path))]

               ;; Environment variable doesn't exist
               (if (empty? env-var-path)
                 (log :debug
                      (str "Not looking for config within $" env-var
                           " since the environment variable is not set")))

               (if (and (nil? config)
                        (has-more-items i env-vars))
                 ;; Keep looking if config wasn't found we have more environment
                 ;; variables to look through
                 (recur (+ 1 i))
                 ;; Otherwise return the config we found, even if it's empty
                 config))))
         config)

       (if (nil? config)
         (log :info "No config file found - using defaults")))))

(def config
  "Contains the global app configuration map.
  
  We store it in an atom so it can be live-reloaded."
  (atom
    (let [cfg (load-config "config.edn")]
      (log :info (str "App config loaded: " cfg))
      cfg)))

(defn reload-config
  "Reload config from one of the predefined user directories."
  []
  (reset! config (load-config "config.edn")))
