(in-package :fileworthy)

;;; Reader Macros --------------------------------------------------------------
(defvar lambda-symbol-defined nil)
(unless lambda-symbol-defined
  (labels ((λ-reader (stream char)
             "Allow the character 'λ' to be used in place of the word 'lambda',
              for brevity's sake."
             (declare (ignore char stream))
             'LAMBDA))
    (set-macro-character #\λ #'λ-reader)
    (setf lambda-symbol-defined t)))
;;; Reader Macros --------------------------------------------------------------

;;; Taken from PCL -------------------------------------------------------------
(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~; and ~:;, ~]~}~]~}"
  "A control string to format a list of items in a friendly manner.
   E.g. '1 and 2', or '1, 2 and 3'.")

(defmacro labeled-time (form)
  "Shows timing info via (time), prefixed with the form it's called for."
  `(progn
    (format *trace-output* "~2&~a" ',form)
    (time ,form)))
;;; Taken from PCL -------------------------------------------------------------

;;; Generic Utils --------------------------------------------------------------
(defmacro 1st (obj)
  "Gets the first item in OBJ if it's a list, otherwise OBJ is simply returned."
  `(if (listp ,obj) (first ,obj) ,obj))

(defmacro 2nd (obj &optional fallback)
  "Gets the second item in OBJ if it's a list of at least two items, otherwise
   FALLBACK."
  `(cond ((atom ,obj)
          ,fallback)
         ((and (listp ,obj) (> (length ,obj) 1))
          (second ,obj))
         (t ,fallback)))

(defmacro last1 (lst)
  "Get the last item in lst. If lst is not a list it simply returns it."
  `(if (listp ,lst)
     (car (last ,lst))
     ,lst))

(defun blank? (str)
  "Determine whether `STR` contains only whitespace characters."
  (cl-ppcre:scan "^\\s+$" str))

(defmacro -> (obj slot)
  "Gets the value of a slot."
  `(slot-value ,obj ',slot))

(defmacro => (obj slot val)
  "Sets the value of a slot."
  `(setf (slot-value ,obj ',slot) ,val))

(defmacro loose-parse-int (str &key (fallback 0))
  "Very lenient parsing of STR to an integer."
  `(cond ((typep ,str 'integer)
          ,str)
         ((empty? ,str)
          ,fallback)
         (t
          (or (parse-integer (if (typep ,str 'string)
                                 ,str
                                 (to-string ,str))
                             :junk-allowed t)
              ,fallback))))

(defmacro sf (control-string &rest args)
  "Convenience macro to format a string. `sf` stands for 'string format'."
  (if (listp control-string)
      `(format nil (format nil "~{~A~}" ,control-string) ,@args)
      `(format nil ,control-string ,@args)))

(defmacro to-string (obj)
  "Shortcut of `PRINC-TO-STRING'"
  `(princ-to-string ,obj))
;;; Generic Utils --------------------------------------------------------------

;;; Empty
(defvar empty/*objects*
  (let ((hash (make-hash-table)))
    (setf (gethash 'pathname hash) #P"")
    hash)
  "A hash table of 'empty' objects, keyed by their respective type.")

(defun empty (type &key unless)
  "Get the registered 'empty' object (if any) unless `UNLESS` is non-nil.
   This is an idiomatic way of ensuring the given `UNLESS` object is always
   non-nil."
  (if (not (null unless))
    (return-from empty unless))

  (if (stringp unless)
    (return-from empty unless))

  (if (eq 'string type)
    (return-from empty ""))

  (gethash type empty/*objects*))

(defun empty=> (obj)
  "Register `OBJ` as the canonical 'empty' instance of it's respective type."
  (setf (gethash (type-of obj) empty/*objects*) obj))

(defgeneric empty? (obj)
  (:documentation
    "Determine whether `OBJ` is considered an 'empty' instance of it's
     respective data-type. E.g. a null object, an empty string, an empty list,
     etc."))

(defmethod empty? ((obj pathname))
  (zerop (length (princ-to-string obj))))

(defmethod empty? ((obj string))
  (zerop (length obj)))

(defmethod empty? ((obj list))
  (zerop (length obj)))

(defmethod empty? ((obj t))
  (or (null obj)
      (eq obj (empty (type-of obj)))))
;;; Empty ----------------------------------------------------------------------

;;; Validation
(defparameter levels '(:success 2
                       :info 1
                       :debug 0
                       :warning -1
                       :error -2
                       :fatal -3))

(defstruct r
  "Encapsulates a 'result' indicating the success/failure stated of a function
   or operation. An optional DATA object includes the 'natural' return type of
   the function."
  (level :info)
  (message "")
  (data nil))

(defun new-r (level &optional msg data)
  "Creates a new R."
  (make-r :level (find level levels) :message msg :data data))

(defun succeeded? (obj)
  "Determine whether OBJ represents a 'successful' object."
  (typecase obj
    (r (>= (or (getf levels (r-level obj)) -1) 0))
    (t obj)))

(defun failed? (obj)
  "Determine whether OBJ represents a 'failed' object."
  (not (succeeded? obj)))
;;; Validation

;;; Logging --------------------------------------------------------------------
(defvar *log-format-time*
  '((:year 4) #\- (:month 2) #\- (:day 2) #\space
    (:hour 2) #\: (:min 2) #\: (:sec 2) #\. :msec
    #\space :timezone)
  "This is used by `logm` as the date/time format.")

(defun logm (level msg &rest msg-args)
  "Logs a message to the console."
  (format t "* ~A~%  * ~A~%  * ~A~%"
          (format-timestring nil (now) :format *log-format-time*)
          level
          (apply #'format nil msg msg-args)))
;;; Logging --------------------------------------------------------------------
