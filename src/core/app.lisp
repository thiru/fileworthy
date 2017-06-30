(in-package :fileworthy)

#||
## APP

* The `APP` struct groups general, high-level app details including
  * `DEBUG`
    * whether the site is running in a debug mode
  * `APP-DIR`
    * the root directory of the app's source/binaries
  * `MIN-PASSWORD-LENGTH`
    * the minimum allowed password length
  * `VERSION`
    * the current version of the app
  * `LAST-UPDATED`
    * the time the app was last updated
    * based on the last write time of the [version file](../version)
  * `WEB-STATIC-DIR`
    * the directory containing static client-side web resources
  * `CONFIG-FILE-PATH`
    * the fully qualified path to the config file
    * which is an instance of `CONFIG`
||#
(defstruct app
  "Contains general, high-level app details."
  (debug t)
  (app-dir (empty 'pathname) :type PATHNAME)
  (min-password-length 4 :type INTEGER)
  (version "0.0" :type STRING)
  (last-updated (empty 'timestamp) :type TIMESTAMP)
  (web-static-dir (empty 'pathname) :type PATHNAME)
  (config-file-path (empty 'pathname) :type PATHNAME))

(empty=> (local-time:encode-timestamp 0 0 0 0 1 1 1))
(empty=> (make-app))

#||
### CREATE-APP

* This function creates an instance of `APP`
  * with all fields properly initialised
* Note that `APP-VERSION` is loaded from a separate [version file](../version)
  * this is partly due to [fileworthy.asd](../fileworthy.asd) needing access to the app version as well
  * and this way we have a single place where the version gets updated
    * and it's easily modified and read from
||#
(defun create-app (debug)
  "Create APP instance."
  (let* ((app-dir (asdf:system-source-directory :fileworthy))
         (version-file-path (asdf:system-relative-pathname
                              :fileworthy
                              "version"))
         (xdg-config-home (xdg-config-home))
         (config-file-name "config")
         (config-file-dir "")
         (config-file-path ""))

    ;; If $XDG_CONFIG_HOME not set, set it to ~/.config
    (if (empty? xdg-config-home)
      (setf xdg-config-home (merge-pathnames* ".config" (user-homedir-pathname))))

    (setf config-file-dir (merge-pathnames* "fileworthy/" xdg-config-home))
    (setf config-file-path (merge-pathnames* config-file-name config-file-dir))

    (when (not (directory-exists-p config-file-dir))
      (format t
              "Creating '~A' as it doesn't exist.~%"
              config-file-dir)
      (ensure-directories-exist config-file-dir))

    (when (not (file-exists-p config-file-path))
      (format t
              "Creating '~A' from default config as it doesn't exist.~%"
              config-file-path)
      (copy-file (merge-pathnames* "config" app-dir) config-file-path))

    (make-app :debug debug
              :app-dir app-dir 
              :version (asdf::read-file-form version-file-path)
              :last-updated
              (universal-to-timestamp
                (file-write-date version-file-path))
              :web-static-dir (merge-pathnames #P"static/" app-dir)
              :config-file-path config-file-path)))
