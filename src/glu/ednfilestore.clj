;; ## Summary
;;
;; A very simple and probably naive way to treate EDN files as a data store.
;;
;; Concurrent access to files are protected by creating lock files. E.g. if
;; we need to update *config.edn*, an attempt will be made to create a file
;; with the same name with a "lock" suffix, e.g. *config.edn.lck*.
;;
;; This namespace does not contain any domain-specific code.
;;
(ns glu.ednfilestore
  (:require
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [clojure.string :as string]

            [glu.logging :refer :all]
            [glu.utils :refer :all]))

(defn load
  "Load EDN data at the given path.

  Returns:
  * The EDN data if successfull
  * `nil` if the file couldn't be found
  * An exception may occur if the file doesn't contain valid EDN"
  [path]
  (if (file-exists? path)
    (-> path slurp edn/read-string)))

(def lock-file-extension
  "File extension for lock files."
  "lck")

(defn _create-lock-file
  "Creates a lock file for the given file. The lock file creation will be
  attempted even if the given target file doesn't exist.

  * `target-file`
    * The file for which the lock is being created

  Returns:
  * `true` if the lock file was successfully created anew
  * `false` if the lock file already exists
  * An exception may occur for other error scenarios
    * e.g. if a directory in the file path of `target-file` doesn't exist"

  [target-file]

  (if (string/blank? target-file)
    (throw (ex-info "No target file provided" {:param :target-file})))

  (let [lock-file-path (fmt "~A.~A" target-file lock-file-extension)
        file (new java.io.File lock-file-path)
        file-created? (.createNewFile file)]
    file-created?))

(defn _delete-lock-file
  "Deletes a lock file for the given file.

  * `target-file`
    * The file for which the lock is being created for

  Returns:
  * `true` if the lock file was successfully deleted
  * `false` otherwise"

  [target-file]

  (if (string/blank? target-file)
    (throw (ex-info "No target file provided" {:param :target-file})))

  (let [lock-file-path (fmt "~A.~A" target-file lock-file-extension)]
    (if (file-exists? lock-file-path)
      (io/delete-file lock-file-path true)
      false)))

(defn save
  "Save (update/create) a file with the given data.

  * `file-path`
    * A string specifying the file path
  * `data`
    * The contents of the file

  Returns:
  * `true` if the file was successfully saved
  * `false` otherwise
  * An exception may occur for other error scenarios
    * e.g. if a directory in the file path of `file-path` doesn't exist"

  [file-path data]

  (if (string/blank? file-path)
    (throw (ex-info "No target file provided" {:param :file-path})))

  (if-not (_create-lock-file file-path)
    false
    (do
      (spit file-path data)
      (_delete-lock-file file-path)
      (file-exists? file-path))))
