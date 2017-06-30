(in-package :fileworthy)

#||
### `PAGE-USER-DETAIL`
||#
(defun page-user-detail ()
  "User details page."
  (let* ((curr-user (empty 'user :unless (session-value 'user)))
         (path-segs (split-sequence #\/ (script-name*) :remove-empty-subseqs t))
         (user-id-str (nth 2 path-segs))
         (new-user? (string-equal "new" user-id-str))
         (req-user (empty
                     'user
                     :unless (get-user :id (loose-parse-int user-id-str)))))
    ;; Redirect to Not Found page if user not found
    (if (and (not new-user?) (empty? req-user))
      (return-from page-user-detail (page-error-not-found)))
    ;; Redirect to Forbidden page if not admin and not current user
    (if (and (not (user-admin? curr-user))
             (not (eq curr-user req-user)))
      (return-from page-user-detail (page-error-not-authorised)))
    (page-template
      (if new-user? "New User" (user-name req-user))
      "user-detail-page"
      (gen-html
        (:h2
          :id "name-heading"
          :data-user-id (to-string (user-id req-user))
         (if new-user? "New User" (user-name req-user)))
        (:div :id "input-fields"
         (:input
           :id "user-name"
           :placeholder "Name"
           :title "Name"
           :type "text"
           :value (user-name req-user))
         (:input
           :id "email-address"
           :placeholder "Email Address"
           :title "Email Address"
           :type "email"
           :value (user-email req-user))
         (if (user-admin? curr-user)
           (htm
               (:input
                 :id "root-dir"
                 :placeholder "Root Folder"
                 :title "Root Folder"
                 :type "text"
                 :value (user-root-dir req-user))))
         (if (user-admin? curr-user)
           (htm
               (:label
                 (if (user-admin? req-user)
                   (htm
                       (:input :id "is-admin" :checked "" :type "checkbox"))
                   (htm
                       (:input :id "is-admin" :type "checkbox")))
                 " Administrator")))
         (:div
           :class (if new-user? "hidden" "")
           (:a
             :id "show-pwds-btn"
             :class "button"
             :href "javascript:page.toggleChangePwd()"
             "Change Password")
           (:a
             :id "hide-pwds-btn"
             :class "button hidden"
             :href "javascript:page.toggleChangePwd()"
             "Don't Change Password"))
         (:div
           :id "password-fields"
           :class (if new-user? "" "hidden")
           (:input
             :id "current-pwd"
             :class (if new-user? "hidden" "")
             :placeholder "Current Password"
             :title "Current Password"
             :type "password")
           (:input
             :id "new-pwd"
             :placeholder "New Password"
             :title "New Password"
             :type "password")
           (:input
             :id "new-pwd-confirm"
             :placeholder "Confirm New Password"
             :title "Confirm New Password"
             :type "password")))
        (:p :id "save-result" "")
        (:a
          :id "save-btn"
          :class "button full-width"
          :href "javascript:page.save()"
          "Save")))))
