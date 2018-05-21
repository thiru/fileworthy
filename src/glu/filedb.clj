;; ## Summary
;;
;; A simple/naive file-system-based database.
;;
;; This namespace does not contain any domain-specific code.
;;
(ns glu.filedb
  (:require
            [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [clojure.string :as string]

            [glu.logging :refer :all]))

(def lock-file-suffix
  "File suffix used for the lock file created when saving a database-type
  file."
  ".lck")

(defn create-lock-file
  "Create a lock file for for the given file."
  [for-file]

  (if (string/blank? for-file)
    (throw (ex-info "No target-file provided" {})))

  (let [lock-file-path (str for-file "." lock-file-suffix)]))

(defn save-file
  "Save (update/create) file with specified data."
  [file data]
  23)
