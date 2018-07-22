(ns glu.fsreload
  "Support automatic reloading of Clojure code by monitoring file-system
   change events."
  (:require
            [clojure.string :as string]

            [hawk.core :as hawk]
            [ns-tracker.core :as nst]

            [glu.core :refer :all]
            [glu.logging :refer :all]))

(def watch-dir
  "The directory to watch for source code changes."
  "src")

(def modified-namespaces
  "Holds modified namespaces."
  (nst/ns-tracker [watch-dir]))

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

(defonce
  ^{:doc "Holds the file-system watcher instance."}
  watcher
  (atom nil))

(defn watcher-filter
  "Filter the type of file-system events we're interested in."
  [_ctx {:keys [kind file]}]
  (and file
       ;; We're only interested in file modified events. I don't think we need
       ;; to care about file deleted events as we can't unload the code anyway
       ;; (as far as I know). And file created events don't seem important
       ;; enough to bother watching.
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
  "Start watching the folder specified by `watch-dir` for file change
   notifications."
  []
  (if-not (nil? @watcher)
    (log :warning (str "File-system watcher already appears to be running - "
                       "not starting a new watcher"))
    (do
      (reset! watcher
              (hawk/watch! [{:paths [watch-dir]
                             :filter watcher-filter
                             :handler watcher-handler}]))
      (log :debug
           (fmt "File-system watcher started watching '~A'" watch-dir)))))

(defn stop-watch!
  "Stop the file-system watcher."
  []
  (if (nil? @watcher)
    (log :warning "File-system watcher doesn't appear to be running")
    (do
      (hawk/stop! @watcher)
      (log :debug "File-system watcher stopped"))))

