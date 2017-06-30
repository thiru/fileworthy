(in-package :fileworthy)

#||
## Web Utils

* This section contains common utility functions for the web-related code
||#

#||
### `CREATE-WEB-ACCEPTOR`

* This function creates the Hunchentoot (easy) acceptor
* See `START-APP` for a description of the parameters
  * as it uses the exact same list
||#
(defun create-web-acceptor (&key (port 9090) (debug t))
  "Creates an 'easy-acceptor' which will listen on the specified port."
  (make-instance 'easy-acceptor
                 :port port
                 :document-root (app-web-static-dir *app*)
                 :access-log-destination (if debug
                                           *standard-output*
                                           "tbnl-access.log")
                 :message-log-destination (if debug
                                            *standard-output*
                                            "tbnl-message.log")))

#||
### `GET-FS-PATH-FROM-URL`
||#
(defun get-fs-path-from-url (user &optional path-name)
  "Gets an absolute local file-system path from the given URL path name."
  (let* ((abs-path (get-abs-user-root-dir user)))
    (if (not (blank? path-name))
      (setf abs-path (join abs-path (trim path-name #\/ :left-only t))))
    abs-path))
(defmacro gen-html (&body body)
  "Generate an HTML string from the given s-exp."
  `(with-html-output-to-string (*standard-output* nil)
    (htm ,@body)))

#||
### `SESSION-COOKIE-NAME`

* Let's use a custom session name
  * rather than the default: "hunchentoot-session"
||#
(defmethod session-cookie-name ((acceptor easy-acceptor))
  "fileworthy-session")

(defun set-http-code (code)
  "Set the current request's HTTP status code to `CODE`."
  (setf (return-code*) code))

(defun url-for (section-or-obj)
  "Create URL for a particular section/object"
  (cond ((eq 'about section-or-obj)
         (sf "/~A/about"
             (config-reserved-resource-path *config*)))
        ((eq 'settings section-or-obj)
         (sf "/~A/settings"
             (config-reserved-resource-path *config*)))
        ((eq 'users section-or-obj)
         (sf "/~A/users"
             (config-reserved-resource-path *config*)))
        ((typep section-or-obj 'user)
         (if (= 0 (user-id section-or-obj))
           (sf "/~A/users/new"
               (config-reserved-resource-path *config*))
           (sf "/~A/users/~A/~(~A~)"
               (config-reserved-resource-path *config*)
               (user-id section-or-obj)
               (user-name section-or-obj))))
        (t "")))

(defun json-result (result &optional data)
  "Converts the given R instance to a JSON string."
  (json:encode-json-plist-to-string
    `(level ,(r-level result)
            message ,(r-message result)
            data ,(or data (r-data result)))))

(defun json-error (status-code)
  "Create a JSON response indicating an error with the specified HTTP status
   code."
  (set-http-code status-code)
  (json:encode-json-plist-to-string
    '(level error
            message "Sorry, you don't have permission to perform this request.")))

(defun set-auth-cookie (name value)
  "Create a secure cookie."
  (set-cookie name
              :value value
              ;; Expire a month from now
              :max-age (* 60 60 24 30)
              :path "/"
              :secure (not (app-debug *app*))
              :http-only t))

(defun parse-js-bool (val)
  "Parse a Javascript boolean taken from a post parameter to a Lisp bool."
  (or (string-equal "true" val)
      (string-equal "1" val)))
