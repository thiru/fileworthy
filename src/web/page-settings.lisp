(in-package :fileworthy)

#||
### `PAGE-SETTINGS`
||#
(defun page-settings ()
  "App settings page."
  (let* ((curr-user (session-value 'user)))
    ;; Only admins can view this page
    (if (or (null curr-user)
            (not (user-admin? curr-user)))
      (return-from page-settings (page-error-not-authorised)))
    (page-template
      "Settings"
      "settings-page"
      (gen-html
        (:h2 "Settings")
        (:ul :id "inputs" :class "flat-list"
         (:li
           (:label
             (:span "Site Name")
             (:input :id "site-name" :value (config-site-name *config*))
             (:div :class "clear-fix")))
         (:li
           (:label
             (:span "Root Folder")
             (:input :id "root-dir" :value (config-root-dir *config*))
             (:div :class "clear-fix")))
         (:li
           (:label
             (:span "Port")
             (:input :id "port" :value (to-string (config-port *config*)))
             (:div :class "clear-fix")))
         (:li
           (:label
             (:span "Allow anonymous read access")
             (if (config-allow-anonymous-read *config*)
               (htm
                   (:input :id "anon-read" :checked "" :type "checkbox"))
               (htm
                   (:input :id "anon-read" :type "checkbox")))
             (:div :class "clear-fix")))
         (:li
           :title (sf '("The glob pattern specifying binary files to search. "
                        "Leaving this blank will search ALL binary files and "
                        "may be VERY SLOW depending on the number and size "
                        "of binary files in your root folder."))
           (:label
             (:span "Binary file search glob")
             (:input
               :id "bin-files-glob"
               :value (config-bin-files-glob *config*))
             (:div :class "clear-fix"))
           )
         (:li
           (:label
             (:span "Reserved Resource Path")
             (:input
               :id "rrp"
               :value (config-reserved-resource-path *config*))
             (:div :class "clear-fix"))))
        (:div :id "save-result" "")
        (:button
          :id "save-btn"
          :class "button full-width"
          :onclick "page.save()"
          "Save")))))

#||
### `API-SETTINGS-SAVE`
||#
(defun api-settings-save ()
  "Settings save API."
  (setf (content-type*) "application/json")
  (let* ((curr-user (empty 'user :unless (session-value 'user)))
         (site-name (post-parameter "siteName"))
         (root-dir (post-parameter "rootDir"))
         (port (loose-parse-int (post-parameter "port")))
         (port-changed? (/= port (config-port *config*)))
         (anon-read? (parse-js-bool (post-parameter "anonRead")))
         (bin-files-glob (post-parameter "binFilesGlob"))
         (rrp (post-parameter "rrp"))
         (rrp-changed? (not (string= rrp
                                     (config-reserved-resource-path *config*))))
         (save-res (new-r :error "Settings save unexpectedly aborted.")))

    ;; Validation
    (if (or (empty? curr-user)
            (not (user-admin? curr-user))) 
      (return-from api-settings-save (json-error +http-forbidden+)))
    (if (not (plusp port))
      (return-from
        api-settings-save
        (json-result (new-r :error "Port must be a positive integer."))))
    (if (blank? rrp)
      (return-from
        api-settings-save
        (json-result (new-r :error "Reserved Resource Path is required."))))

    ;; Persist
    (setf save-res
          (save-config
            (λ ()
               (setf (config-site-name *config*) site-name)
               (setf (config-root-dir *config*) root-dir)
               (setf (config-port *config*) port)
               (setf (config-allow-anonymous-read *config*) anon-read?)
               (setf (config-bin-files-glob *config*) bin-files-glob)
               (setf (config-reserved-resource-path *config*) rrp))))

    ;; Return success/failure
    (if (succeeded? save-res)
      (progn
        (setf *config* (load-config (app-config-file-path *app*)))
        (if rrp-changed?
          (define-routes))
        ;; TODO: automatically reset if the port changed
        (if port-changed?
          (json-result
            (new-r :success (sf '("Config updated. Please restart the app to "
                                  "use the new port."))))
          (json-result save-res)))
      (json-result save-res))))

