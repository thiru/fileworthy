(in-package :cl-user)

(defpackage :glu
  (:use :cl :local-time)
  (:export
    :*english-list*
    :empty? :non-empty?
    :labeled-time
    :last1
    :levels
    :loose-parse-int
    :r :r-data :r-level :r-message :new-r :succeeded? :failed?
    :sf
    :to-string
    ))

(in-package :glu)

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
(defmacro last1 (lst)
  "Get the last item in lst. If lst is not a list it simply returns it."
  `(if (listp ,lst)
     (car (last ,lst))
     ,lst))

(defmacro -> (obj slot)
  "Gets the value of a slot."
  `(slot-value ,obj ',slot))

(defmacro => (obj slot val)
  "Sets the value of a slot."
  `(setf (slot-value ,obj ',slot) ,val))

(defmacro empty? (val)
  "Determine whether 'val' is considered empty. I.e. is null, an empty
   sequence, string or path."
  `(or (null ,val)
       (and (typep ,val 'PATHNAME) (= 0 (length (princ-to-string ,val))))
       (and (listp ,val) (= 0 (length ,val)))
       (and (stringp ,val) (= 0 (length, val)))))

(defmacro non-empty? (val)
  "Determine whether VAL is not empty. I.e. it is either a non-nil atom, a
   non-empty list, or a non-empty string."
  `(not (empty? ,val)))

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
