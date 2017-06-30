(in-package :fileworthy)

#||
### `API-USER-SAVE`
||#
(defun api-user-save ()
  "User save API."
  (setf (content-type*) "application/json")
  (let* ((curr-user (empty 'user :unless (session-value 'user)))
         (path-segs (split-sequence #\/ (script-name*) :remove-empty-subseqs t))
         (id (loose-parse-int (nth 3 path-segs)))
         (new-user? (zerop id))
         (req-user (empty 'user :unless (get-user :id id)))
         (name (post-parameter "name"))
         (email (post-parameter "email"))
         (root-dir (or (post-parameter "rootDir") ""))
         (admin? (and (user-admin? curr-user)
                      (parse-js-bool (post-parameter "isAdmin"))))
         (current-pwd (post-parameter "currentPwd"))
         (new-pwd (post-parameter "newPwd"))
         (save-res (new-r :error "User save unexpectedly aborted.")))

    ;; Validation
    (if (empty? curr-user)
      (return-from api-user-save (json-error +http-forbidden+)))
    (if (and (not new-user?) (empty? req-user))
      (return-from
        api-user-save
        (json-result (new-r :error "User with id ~A not found." id))))
    ;; Non-admins cannot change another user's password
    (if (and (not new-user?)
             (not (user-admin? curr-user))
             (/= (user-id curr-user) (user-id req-user)))
      (return-from
        api-user-save
        (json-error +http-forbidden+)))
    (if (empty? name)
      (return-from
        api-user-save
        (json-result (new-r :error "No user name provided."))))
    (if (empty? email)
      (return-from
        api-user-save
        (json-result (new-r :error "No email address provided."))))
    (if (and (empty? new-pwd)
             (or new-user?
                 (and (not new-user?) (not (empty? current-pwd)))))
      (return-from
        api-user-save
        (json-result (new-r :error "No password provided."))))
    (if (and (blank? new-pwd)
             (or new-user?
                 (and (not new-user?) (not (empty? current-pwd)))))
      (return-from
        api-user-save
        (json-result (new-r :error "Password can't be blank."))))
    (if (and (> (app-min-password-length *app*) (length new-pwd))
             (or new-user?
                 (and (not new-user?) (not (empty? current-pwd)))))
      (return-from
        api-user-save
        (json-result
          (new-r :error
                 (sf "Password must be at least ~A characters."
                     (app-min-password-length *app*))))))
    (if (and (not new-user?)
             (not (user-admin? curr-user))
             (not (empty? new-pwd))
             (not (authenticate-user req-user current-pwd)))
      (return-from
        api-user-save
        (json-result
          (new-r :error "Current password is incorrect."))))

    ;; Persist
    (setf save-res
          (save-config
            (λ ()
               (let* ((curr-config *config*)
                      (salt (random-string)))
                 (if new-user?
                   (progn
                     (push
                       (make-user
                         :id (config-next-user-id curr-config)
                         :name name
                         :email email
                         ;; Only admins can change a user's root dir
                         :root-dir (if (user-admin? curr-user)
                                     root-dir
                                     (user-root-dir req-user))
                         :admin? admin?
                         :salt salt
                         :password (gen-hash new-pwd salt))
                       (config-users curr-config))
                     (incf (config-next-user-id curr-config)))
                   (progn
                     (setf (user-name req-user) name)
                     (setf (user-email req-user) email)
                     (if (user-admin? curr-user)
                       (setf (user-root-dir req-user) root-dir))
                     (setf (user-admin? req-user) admin?)
                     (when (not (empty? new-pwd))
                       (setf (user-salt req-user) salt)
                       (setf (user-password req-user)
                             (gen-hash new-pwd salt)))))))))

    (if (failed? save-res)
      (return-from api-user-save (json-result save-res)))

    ;; If the password changed for an existing user, remove all session
    ;; objects for the user, except the current session
    (when (and (not new-user?)
               (not (empty? new-pwd)))
      (hunchentoot::with-session-lock-held
        ((session-db-lock *acceptor*))
        (setf (session-db *acceptor*)
              (loop :for (k . v) in (session-db *acceptor*)
                    :when (or (= (session-id v)
                                 (session-id *session*))
                              (/= (user-id req-user)
                                  (user-id (session-value 'user v))))
                    :collect (cons k v)))))

    ;; Return success
    (json-result (new-r :success
                        (if new-user?
                          (sf "Saved new user, ~A." name)
                          (sf "Updated ~A's account." name))))))
