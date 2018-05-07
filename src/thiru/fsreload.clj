;; ## Summary
;;
;; Support automatic reloading of Clojure code by monitoring file-system
;; change events.
;;
;; This namespace does not contain any domain-specific code.
;;
(ns thiru.fsreload
  (:require
            [clojure.string :as string]

            [hawk.core :as hawk]
            [ns-tracker.core :as nst]

            [thiru.logging :refer :all]))

(def modified-namespaces
  "Contains modified namespaces."
  (nst/ns-tracker ["src"]))

(defn reload!
  "Reload modified namespaces.

  * `fs-event`
    * A map describing the file-system event that occurred"
  [fs-event]
  (let [namespaces (modified-namespaces)]
    (when (and (:file fs-event) (pos? (count namespaces)))
      (log :debug
           (format "%s: \"%s\""
                   (string/upper-case (name (:kind fs-event)))
                   (.getPath (:file fs-event))))
      (doseq [ns-sym namespaces]
        (log :debug (str "(require " ns-sym " :reload)"))
        (require ns-sym :reload)))))

;;; Holds the file-system watcher object.
(defonce watcher (atom nil))

(defn watcher-filter
  "Filter the type of file-system events we're interested in."
  [_ctx {:keys [kind file]}]
  (and file
       ;; Only interested in file modified events. I don't think we need to
       ;; care about file deleted events as we can't unload the code any (as
       ;; far as I know). And file created events don't seem important enough
       ;; bother watching.
       (= :modify kind)
       ;; Ignore directory events
       (.isFile file)
       (not (.isHidden file))
       (let [file-name (.getName file)]
         ;; Ignore hidden/temporary files
         (and (not= \. (first file-name))
              (not= \# (first file-name))
              (not= \~ (last file-name))
              ;; Only interested in Clojure file types
              (or (string/ends-with? file-name "clj")
                  (string/ends-with? file-name "cljc")
                  (string/ends-with? file-name "cljs"))))))

(defn watcher-handler
  "Handler for the Hawk file-system watcher.
  This performs the code reloading."
  [ctx e]
  (reload! e)
  ctx)

(defn start-watch!
  "Start watching the *src* folder for file change notifications."
  []
  (if-not (nil? @watcher)
    (log :warning (str "File-system watcher already appears to be running - "
                       "not starting a new watcher"))
    (do
      (reset! watcher
              (hawk/watch! [{:paths ["src"]
                             :filter watcher-filter
                             :handler watcher-handler}]))
      (log :debug "File-system watcher started watching 'src'"))))

(defn stop-watch!
  "Stop the file-system watcher."
  []
  (if (nil? @watcher)
    (log :warning "File-system watcher doesn't appear to be running")
    (do
      (hawk/stop! @watcher)
      (log :debug "File-system watcher stopped"))))
