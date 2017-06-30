(in-package :fileworthy)

#||
### `API-FS-SEARCH`
||#
(defun api-fs-search ()
  "File-system search API."
  (setf (content-type*) "application/json")
  (let* ((user (empty 'user :unless (session-value 'user)))
         (user-root-dir-length (length (get-abs-user-root-dir user)))
         (search-path (post-parameter "search-path"))
         (search-type (post-parameter "search-type"))
         (search-txt (post-parameter "search"))
         (abs-search-path (merge-pathnames*
                            (string-left-trim '(#\/) search-path)
                            (get-abs-user-root-dir user)))
         (search-result nil))

    (log-message* :info "*** Absolute search path: ~A" abs-search-path)

    ;; Check anonymous access
    (if (and (empty? user)
             (not (config-allow-anonymous-read *config*)))
      (return-from api-fs-search (json-error +http-forbidden+)))

    (setf search-txt
          ;; Show all files if search text is "*"
          (if (string-equal "*" search-txt)
            ""
            ;; Maybe being overly cautious on allowed characters
            (ppcre:regex-replace-all "[^a-zA-Z0-9\\-_ \\./]+" search-txt "")))

    ;; Get search results (in absolute path form)
    (setf search-result
          ;; Search file names by default
          (cond ((string-equal search-type "text")
                 (search-file-content search-txt
                                      :path abs-search-path))
                ((string-equal search-type "text+binary")
                 (search-file-content search-txt
                                      :path abs-search-path
                                      :search-binary? t))
                (t
                 (search-file-names search-txt abs-search-path))))

    ;; Trim absolute path segment
    (setf (r-data search-result)
          (map 'list
               (λ (x)
                  (if (empty? x)
                    x
                    (subseq x user-root-dir-length)))
               (r-data search-result)))

    (json-result search-result)))

(defun run-cmd (cmd)
  "Run command specified by `CMD'.
   A result object is returned."
  (multiple-value-bind (std-out std-err ret-val)
      (uiop:run-program cmd
                        :ignore-error-status t
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t))
    (if (zerop ret-val)
        (new-r :success "" std-out)
        (new-r :error
               (sf "ERROR ~A: ~A"
                   ret-val
                   (if (and (empty? std-out) (empty? std-err))
                       "unknown (cmd reported no info)"
                       (or std-err std-out)))))))

#||
#### `SEARCH-FILE-NAMES`

* This runs rg (ripgrep) to search for file names
* rg command arguments include
  * `--files`: only match pattern against filenames (i.e. not file content)
  * `--follow`: follow symlinks
  * `--glob`: include/exclude file pattern
  * `--ignore-case`: case insensitive search
* TODO: Add support for case-insensitive glob pattern matching
  * maybe this can be done by replacing every letter like so: "test" -> "[Tt][Ee][Ss][Tt]"
||#
(defun search-file-names (pattern &optional path)
  "Search for files matching `pattern` at `path`."
  (let* ((cmd (sf "rg --follow --ignore-case --glob '~A' --files ~A"
                  (if (empty? pattern) pattern (sf "*~A*" pattern))
                  (or path "")))
         (search-result nil))
    (log-message* :info "Filename search cmd: ~A" cmd)
    (setf search-result (run-cmd cmd))
    (if (succeeded? search-result)
      (setf (r-data search-result)
            (sort (split-sequence #\linefeed (r-data search-result))
                  #'string-lessp)))
    search-result))

#||
#### `SEARCH-FILE-CONTENT`

* This runs rg (ripgrep) to search for files containing some pattern
* rg command arguments include
  * `--files-with-matches`: only output filenames (i.e. don't output matching content within file)
  * `--fixed-strings`: treat `pattern` as a literal string instead of a regular expression
  * `--follow`: follow symlinks
  * `--glob`: include/exclude file pattern
  * `--ignore-case`: case insensitive search
  * `--text`: search binary files as if they were text (if applicable)
||#
(defun search-file-content (pattern &key path search-binary?)
  "Search for files containing text matching `pattern` within the `path`. Optionally search
   binary files."
  (let* ((text-search-cmd
           (sf '("rg --files-with-matches --fixed-strings --follow"
                 " --ignore-case '~A' ~A")
               pattern
               (or path "")))
         (bin-search-cmd
           (sf '("rg --files-with-matches --fixed-strings --follow"
                 " --glob '~A' --ignore-case --text '~A' ~A")
               (config-bin-files-glob *config*)
               pattern
               (or path "")))
         (text-search-result (new-r :info ""))
         (bin-search-result (new-r :info ""))
         (file-matches '()))

    ;; First search text files
    (log-message* :info "Text file content search cmd: ~A" text-search-cmd)
    (setf text-search-result (run-cmd text-search-cmd))
    (if (succeeded? text-search-result)
      (setf file-matches
            (split-sequence #\linefeed (r-data text-search-result))))
    (format t "TEXT-MATCHES: ~A~%" file-matches)

    ;; Then search binary files
    (when search-binary?
      (log-message* :info "Binary file content search cmd: ~A" bin-search-cmd)
      (setf bin-search-result (run-cmd bin-search-cmd))
      (if (succeeded? bin-search-result)
        (setf file-matches
              (append file-matches
                      (split-sequence #\linefeed (r-data bin-search-result))))))

    ;; Join results and sort
    (if (empty? file-matches)
      (new-r :error
             (string-trim
               " "
               (concatenate 'string
                            (r-message text-search-result)
                            " "
                            (r-message bin-search-result))))
      (new-r :success "" (sort file-matches #'string-lessp)))))
