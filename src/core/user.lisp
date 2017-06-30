(in-package :fileworthy)

#||
### USER

* This struct encapsulates a user account
||#
(defstruct user
  (id 0 :type INTEGER)
  (name "" :type STRING)
  (email "" :type STRING)
  (root-dir "" :type STRING) 
  (password "" :type STRING)
  (salt "" :type STRING)
  (admin? nil :type BOOLEAN))

(empty=> (make-user))

(defun get-user (&key (id 0 id-given?) name email)
  "Get user with one or more of the specified parameters."
  (if (or id-given? name email)
    (find-if (λ (user)
                (and (or (not id-given?) (= id (user-id user)))
                     (or (null name) (string-equal name (user-name user)))
                     (or (null email) (string-equal email (user-email user)))))
             (config-users *config*))))

(defun get-abs-user-root-dir (user)
  "Get the absolute root directory for the specified user."
  (if user
    (let* ((abs-dir (config-root-dir *config*)))
      (if (not (blank? (user-root-dir user)))
        (setf abs-dir
              (sf "~A~A/"
                  abs-dir
                  (string-trim '(#\/) (user-root-dir user)))))
      abs-dir)))

(defun authenticate-user (user pwd)
  "Authenticate the given user."
  (and user
       (not (blank? pwd))
       (string= (gen-hash pwd (user-salt user))
                (user-password user))))

