;; ## Summary
;;
;; High-level app details and configuration, used in places like the CLI, web
;; server defaults, build definition, etc.
;;
(ns fileworthy.app
  (:require
            [glu.logging :refer :all]
            [glu.utils :refer :all]))

(def config-defaults
  "Default configuration values.

  This will be used if a user supplied config can't be found."
  {:name "Fileworthy"
   :version "0.0.1"})

(def config
  "Contains the global app configuration map.

  We store it in an atom so it can be live-reloaded."
  (atom
    (let [config-res (load-config "config.edn" config-defaults)]
      (log nil config-res)
      (:data config-res))))

(defn reload-config
  "Reload config from one of the predefined user directories.

  * `file-name`
    * If specified, it is will be used as the config file name
    * Otherwise 'config.edn' is used"
  ([] (reload-config "config.edn"))
  ([file-name]
   (reset! config (load-config file-name config-defaults))))
