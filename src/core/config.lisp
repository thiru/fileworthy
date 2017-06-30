(in-package :fileworthy)

#||
### `CONFIG`

* This struct encapsulates user-configurable settings
  * `SITE-NAME`
    * the name for the website
  * `ROOT-DIR`
    * the root directory from which the website is generated
  * `ALLOW-ANONYMOUS-READ`
    * whether users that are not logged in are able to access the website
  * `BIN-FILES-GLOB`
    * glob pattern to be used when searching within binary files
    * this is used by ripgrep
      * which currently supports .gitignore style glob patterns
  * `RESERVED-RESOURCE-PATH`
    * the path within the site that is reserved for app-specific resources
    * essentially everything outside of a file-system path designation:
      * static files (javascript, css, etc)
      * admin pages
      * other custom pages
  * `NEXT-USER-ID`
    * holds the identifier that will be used for the next new user
  * `USERS`
    * all registered users
||#

(defstruct config
  (site-name "" :type STRING)
  (root-dir "" :type STRING)
  (port 0 :type INTEGER)
  (allow-anonymous-read t)
  (bin-files-glob)
  (reserved-resource-path "" :type STRING)
  (next-user-id 1 :type INTEGER)
  (users '() :type LIST))

(empty=> (make-config))

;; TODO: expand environment variables in ROOT-DIR
(defun load-config (path)
  "Load an instance of `CONFIG` from the config file."
  (let* ((config (read-file-form path)))
    ;; If no root dir is specified, use user's home dir
    (if (blank? (config-root-dir config))
      (setf (config-root-dir config)
            (to-string (uiop/common-lisp:user-homedir-pathname))))
    (if (not (char-equal #\/
                         (char (config-root-dir config)
                               (1- (length (config-root-dir config))))))
      (setf (config-root-dir config)
            (sf "~A/" (config-root-dir config))))
    (if (not (directory-exists-p (config-root-dir config)))
      (error (sf "Directory '~A' not found or inaccessible."
                 (config-root-dir config))))
    config))
