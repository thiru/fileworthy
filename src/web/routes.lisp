(in-package :fileworthy)

#||
## Web Resource Routes

* Route are defined in a function since
  * we need to have the `*APP*` instance initialised first
  * the page functions are defined below this point
||#
(defun define-routes ()
  "Define web resource routes."
  (setq *dispatch-table*
        (list
          ;; Static files
          (create-folder-dispatcher-and-handler
            (sf "/~A/css/"
                (config-reserved-resource-path *config*))
            (merge-pathnames* "css/" (app-web-static-dir *app*)))
          (create-folder-dispatcher-and-handler
            (sf "/~A/deps/"
                (config-reserved-resource-path *config*))
            (merge-pathnames* "deps/" (app-web-static-dir *app*)))
          (create-folder-dispatcher-and-handler
            (sf "/~A/js/"
                (config-reserved-resource-path *config*))
            (merge-pathnames* "js/" (app-web-static-dir *app*)))

          ;; About page
          (create-regex-dispatcher
            (sf "^/~A/about/?$"
                (config-reserved-resource-path *config*))
            #'page-about)

          ;; Settings page
          (create-regex-dispatcher
            (sf "^/~A/settings/?$"
                (config-reserved-resource-path *config*))
            #'page-settings)

          ;; Settings save API
          (create-regex-dispatcher
            (sf "^/~A/api/settings/?$"
                (config-reserved-resource-path *config*))
            #'api-settings-save)

          ;; User list page
          (create-regex-dispatcher
            (sf "^/~A/users/?$"
                (config-reserved-resource-path *config*))
            #'page-user-list)

          ;; User detail page
          (create-regex-dispatcher
            (sf "^/~A/users/.+/?$"
                (config-reserved-resource-path *config*))
            #'page-user-detail)

          ;; User save API
          (create-regex-dispatcher
            (sf "^/~A/api/users/.+/?$"
                (config-reserved-resource-path *config*))
            #'api-user-save)

          ;; Login API
          (create-regex-dispatcher
            (sf "^/~A/api/login/?$"
                (config-reserved-resource-path *config*))
            #'api-login)

          ;; Logout page
          (create-regex-dispatcher
            (sf "^/~A/logout/?$"
                (config-reserved-resource-path *config*))
            #'page-logout)
          
          ;; File-system search API
          (create-regex-dispatcher
            (sf "^/~A/api/search/?$" (config-reserved-resource-path *config*))
            #'api-fs-search)

          ;; File-system path page (this should be the last entry)
          (create-regex-dispatcher
            "^/*"
            #'page-fs-path))))
