;; ## Summary
;;
;; High-level properties of the project, used in places like the CLI, web
;; server defaults, build definition, etc.
;;
(ns fileworthy.config
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [common.utils :refer :all]))

(defn get-config-dir
  "Get (user) directory to store config files."
  []
  (->
   (or (System/getenv "XDG_CONFIG_HOME")
       (System/getenv "HOME")
       (System/getenv "USERPROFILE")
       ".")
   (io/file "fileworthy")
   (.getPath)))
  

(defn load-config-from
  "Load an EDN config file from the given path."
  [path]
  (log :debug (str "Attempting to load config at '" path "'..."))
  (let [file (io/as-file path)
        file-exists (.exists file)
        config (if file-exists
                   (-> path slurp edn/read-string)
                   {})]
    (cond
      (non-empty? config)
      (log :success (str "Config found at '" path "'"))

      (not file-exists)
      (log :debug (str "Config not found at '" path "'"))

      (and file-exists (empty? config))
      (log :debug
           (str "Config found at '"
                path
                "' but it's empty, and so can't be used"))
      
      :else
      (throw (Exception. "Unexpected :else reached in (cond)")))
    config))

(defn load-config
  "Attempt to load *config.edn* from the following locations (in order):
  
  * Current working directory
  * User's config directory
    * Using the XDG_CONFIG_HOME environment variable
    * E.g. *~/.config/fileworthy*
  * User's home directory
    * Using the HOME (\\*nix) or USERPROFILE (Windows) environment variables
    * E.g. *~/.fileworthy*"
  [file-name]
  (let [config (load-config-from file-name)]
    (if (non-empty? config)
      config
      (let [env-vars ["XDG_CONFIG_HOME" "HOME" "USERPROFILE"]]
        (loop [i 0]
          (let [env-var (nth env-vars i)
                env-var-path (System/getenv env-var)
                file-path (if (non-empty? env-var-path)
                            (io/file env-var-path "fileworthy" file-name))
                config (and file-path
                            (load-config-from file-path))]

            ;; Environment variable doesn't exist
            (if (empty? env-var-path)
              (log :debug
                   (str "Not looking for config within $" env-var
                        " since the environment variable is not set")))

            ;; Config found but is empty
            (if (empty? config)
              (log :debug
                   (str "Config found at '" file-path "' but it's empty")))

            (if (and (has-more-items i env-vars)
                     (empty? config))
              ;; Keep looking
              (recur (+ 1 i))
              (if (non-empty? config)
                ;; Success! Non-empty config found.
                config
                ;; Abort if all config locations exhausted without success
                (throw (Exception.
                         (str "Config not found in any of the expected "
                              "locations, including the current working "
                              "directory")))))))))))

(def config
  "Contains the global app configuration map."
  (atom (load-config "config.edn")))

(defn reload-config
  "Reload config atom from one of the predefined user directories."
  []
  (reset! config (load-config "config.edn")))
