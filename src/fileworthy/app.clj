;; ## Summary
;;
;; High-level properties of the project, used in places like the CLI, web
;; server defaults, build definition, etc.
;;
(ns fileworthy.app)

(def info
  "High-level app info/metadata. These properties are more on the descriptive
  side and do not affect the state or behaviour of the app in any meaningful
  way."
  {:name "Fileworthy"
   :version "0.22.0"
   :description (str "Fileworthy is a simple website to manage your notes and "
                     "files across all your devices")})

(def config
  "App configuration and defaults."
  {:log-level-default :info
   :port-default 8023})
