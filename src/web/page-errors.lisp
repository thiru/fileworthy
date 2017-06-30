(in-package :fileworthy)

#||
### Error pages
||#
(defun page-error-not-found ()
  "Not found error page."
  (set-http-code +http-not-found+)
  (page-template
    "Not Found"
    "not-found-page"
    (gen-html
      (:h2 "Not Found")
      (:p "The page or resource you requested could not be found.")
      (if (not (string-equal "/" (script-name* *request*)))
        (htm
          (:p
            (:a :href "/"
             (:i :class "fa fa-home" "")
             (:b " Go back to the home page"))))))))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 404)) &key)
  (page-error-not-found))

(defun page-error-not-authorised ()
  "Not authorised error page."
  (set-http-code +http-forbidden+)
  (page-template
    "Not Authorised"
    "not-authorised-page"
    (gen-html
      (:h2 "Not Authorised")
      (:p "Sorry, you don't have permission to view this page or resource.")
      (if (not (string-equal "/" (script-name* *request*)))
        (htm
          (:p
            (:a :href "/"
             (:i :class "fa fa-home" "")
             (:b " Go back to the home page"))))))))

(defun page-error-server ()
  "Internal server error page."
  (set-http-code +http-internal-server-error+)
  (page-template
    "Server Error"
    "server-error-page"
    (gen-html
      (:h2 "Server Error")
      (:p (sf '("Sorry, it looks like something went wrong on the server. "
                "Please try again later if the problem persists.")))
      (if (not (string-equal "/" (script-name* *request*)))
        (htm
          (:p
            (:a :href "/"
             (:i :class "fa fa-home" "")
             (:b " Go back to the home page"))))))))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 500)) &key)
  (page-error-server))

