(in-package :fileworthy)

#||
### `PAGE-USER-LIST`
||#
(defun page-user-list ()
  "User listing page."
  (let* ((curr-user (session-value 'user)))
    ;; Only admins can view this page
    (if (or (null curr-user)
            (not (user-admin? curr-user)))
      (return-from page-user-list (page-error-not-authorised)))
    (page-template
      "Users"
      "user-list-page"
      (gen-html
        (:a
          :id "new-user-btn"
          :class "button"
          :href (url-for (empty 'user))
          "New User")
        (:ul :class "big-list"
          (loop
            :for user :in (config-users *config*)
            :collect
            (htm
              (:li
                (:a
                  :href (url-for user)
                  (str (user-name user)))))))))))

