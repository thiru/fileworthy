(in-package :fileworthy)

#||
### `PAGE-LOGOUT`
||#
(defun page-logout ()
  (when *session*
    (delete-session-value 'user)
    (remove-session *session*))
  (page-template
    "Logout"
    "logout-page"
    (gen-html
      (:h2 "Thank you, come again!")
      (:p
        (:a :class "full-width"
         :href "/"
         "Go back to Home page")))))

