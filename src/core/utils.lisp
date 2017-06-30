(in-package :fileworthy)

#||
## Domain Utils

### `RANDOM-STRING`

* The Iron Clad function is wrapped in `TO-STRING` so that it prints like a
  regular string (e.g. used when saving config to disk)
  * otherwise the printed representation has "COERCE..."
||#
(defun random-string (&optional (n 16))
  "Return a random hex string with N digits."
  (to-string (ironclad:byte-array-to-hex-string (ironclad:make-random-salt n))))

#||
### `GEN-HASH`

* The Iron Clad function is wrapped in `TO-STRING` so that it prints like a
  regular string (e.g. used when saving config to disk)
  * otherwise the printed representation has "COERCE..."
||#
(defun gen-hash (to-hash &optional salt)
  "Generate a hash of TO-HASH."
  (to-string
    (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence :sha512
                                (ironclad:ascii-string-to-byte-array
                                  (sf "~A~A" to-hash (or salt "")))))))

#||
### `GET-DIR-NAMES`

* This function gets a list of directory names relative to either
  * the given directory, `PARENT`
  * or the root working folder as specified by `CONFIG-ROOT-DIR`
    * and possibly further restricted by the user's root dir: `USER-ROOT-DIR`
||#
(defun get-dir-names (curr-user &optional (parent ""))
  "Gets directory names."
  (map 'list
       (λ (abs-dir)
          (last1 (split-sequence #\/
                                 (princ-to-string abs-dir)
                                 :remove-empty-subseqs t)))
       (uiop/filesystem:subdirectories
         (concatenate 'string
                      (config-root-dir *config*)
                      (string-trim '(#\/) (user-root-dir curr-user))
                      "/"
                      (string-left-trim '(#\/) parent)))))
#||
### `GET-FILE-NAMES`
||#
(defun get-file-names (dir)
  "Gets a list of file names at `DIR`."
  (map 'list
       (λ (abs-file)
          (last1 (split-sequence #\/
                                 (princ-to-string abs-file)
                                 :remove-empty-subseqs t)))
       (uiop/filesystem:directory-files dir)))

#||
### `GET-FILE-CONTENT`
||#
(defun get-file-content (path)
  "Get file contents of `PATH`."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (or data ""))))

#||
### `PRETTY-TIME`
||#
(defun pretty-time (time)
  "Formats a date/time to a user-friendly form. TIME is expected to either be a
   timestamp readable by LOCAL-TIME, or a LOCAL-TIME:TIMESTAMP object."
  (if (empty? time)
      ""
      (let* ((format-desc '())
             (timestamp (if (stringp time)
                            (parse-timestring time)
                            time)))

        (setf format-desc '(:short-weekday " " :short-month " " :day " "
                            :year ", " :hour12 ":" (:min 2) " " :ampm))

        (format-timestring nil timestamp :format format-desc))))
#||
### `IS-FILE-BINARY?`

* This function attempts to determine whether the given path is a binary file
* It does this with a very simple technique of looking for a 0 byte
* This technique should work with ASCII and UTF-8 files
  * but not UTF-16 and UTF-32
||#
(defun is-file-binary? (path)
  "Try to detect if PATH is a binary file."
  (with-open-file (stream path
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist nil)
    (loop :for i :from 0
          :for b = (read-byte stream nil nil)
          :while b
          :when (zerop b)
          :do (return-from is-file-binary? t)))
  nil)

#||
### `CREATE-CONFIG-LOCK-FILE`
||#
(defun create-config-lock-file ()
  "Create a file indicating that the config file is locked."
  (let* ((lock-file (sf "~A.lck" (app-config-file-path *app*))))
    (with-open-file (stream
                      lock-file
                      :direction :output
                      :if-exists nil
                      :if-does-not-exist :create)
      (if (null stream)
        (new-r :warning "Config file is already locked.")
        (progn
          (format stream "~A" (get-universal-time))
          (new-r :success "Config file lock created."))))))

#||
### `DELETE-CONFIG-LOCK-FILE
||#
(defun delete-config-lock-file ()
  "Delete config lock file."
  (delete-file-if-exists (sf "~A.lck" (app-config-file-path *app*))))

#||
### `SAVE-CONFIG`

* `CHANGE-FN` is a function that performs the work of making changes to the
  global `*CONFIG*` object
* This function is only called if the config file lock is successfully obtained
||#
(defun save-config (change-fn)
  "Save/update config file with contents of `*CONFIG*`."
  (let* ((lockedR (create-config-lock-file)))
    (if (failed? lockedR)
      (return-from save-config lockedR))
    (with-open-file (stream
                      (app-config-file-path *app*)
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
      (funcall change-fn)
      (write *config* :stream stream :readably t))
    (delete-config-lock-file))
  (new-r :success "Config updated."))
