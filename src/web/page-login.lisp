(in-package :fileworthy)

#||
### `API-LOGIN`
||#
(defun api-login ()
  "User login API."
  (setf (content-type*) "application/json")
  (let* ((email (post-parameter "email"))
         (pwd (post-parameter "pwd"))
         (user (get-user :email email)))
    (if (empty? email)
      (return-from
        api-login
        (json-result (new-r :error "No email address provided."))))
    (if (empty? pwd)
      (return-from
        api-login
        (json-result (new-r :error "No password provided."))))
    (when (not (authenticate-user user pwd))
      (sleep 2)
      (return-from
        api-login
        (json-result (new-r :error "Incorrect credentials."))))

    ;; Create session for user
    (setf (session-value 'user) user)

    (json-result (new-r :success (sf "Welcome ~A." (user-name user))))))

