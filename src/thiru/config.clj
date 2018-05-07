;; ## Summary
;;
;; General config file management.
;;
;; This namespace does not contain any domain-specific code, and so should be
;; easy to use in other projects.
;;
(ns thiru.config
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thiru.utils :refer :all]
            [thiru.logging :refer :all]
            [thiru.reporting :refer :all]))

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
  "Attempt to load an EDN config file.

  * `file-name`
    * The name of the config file
  * `config-defaults`
    * A map specifying default/fallback values

  The following directories are search (in order):

  * Current working directory
  * User's config directory
    * Using the XDG_CONFIG_HOME environment variable
    * E.g. *~/.config/mflows*
  * User's home directory (\\*nix)
    * Using the HOME environment variable
    * This may also exist in Windows, e.g. Git Bash or WSL
    * E.g. *~/mflows*
  * User's home directory on Windows
    * Using the USERPROFILE environment variable
    * E.g. *C:\\Users\\Me\\mflows*"
  [file-name config-defaults]
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
                               (io/file env-var-path "mflows" file-name))
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

