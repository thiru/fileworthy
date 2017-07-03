;;;; Global Lisp Utilities

(in-package :glu)

;;; Reader Macros --------------------------------------------------------------
(defvar lambda-symbol-defined nil)
(unless lambda-symbol-defined
  (labels ((λ-reader (stream char)
             "Allow the lambda character 'λ' to be used in place of the word,
              for brevity's sake."
             (declare (ignore char stream))
             'LAMBDA))
    (set-macro-character #\λ #'λ-reader)
    (setf lambda-symbol-defined t)))
;;; Reader Macros --------------------------------------------------------------

;;; Global Variables -----------------------------------------------------------
(defvar *start-time* (get-internal-real-time))

(defvar *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~; and ~:;, ~]~}~]~}"
  "A control string to format a list of items in a friendly manner.
   E.g. '1 and 2', or '1, 2 and 3'.
   This was taken from the book Practical Common Lisp.")
;;; Global Variables -----------------------------------------------------------

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
  (or (empty? str)
      (cl-ppcre:scan "^\\s+$" str)))

(defmacro join (str1 str2 &rest args)
  "Concatenate the given strings."
  `(concatenate 'string ,str1 ,str2 ,@args))

(defmacro trim (char str &key left-only right-only)
  "Trim `STR` of `CHAR`."
  `(cond (,left-only
           (string-left-trim (list ,char) ,str))
         (,right-only
           (string-right-trim (list ,char) ,str))
         (t
          (string-trim (list ,char) ,str))))

(defmacro starts-with (test input)
  "Test whether `input` starts with `test`."
  `(string= ,test
            (subseq ,input 0 (min (length ,input) (length ,test)))))

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

(defmacro labeled-time (form)
  "Shows timing info via (time), prefixed with the form it's called for.
   This was taken from the book Practical Common Lisp."
  `(progn
    (format *trace-output* "~2&~a" ',form)
    (time ,form)))

(defun get-run-time ()
  "Get the amount of time elapsed since the program started in seconds."
  (/ (- (get-internal-real-time) *start-time*) internal-time-units-per-second))

(defun display-run-time (&optional app-name
                                   (total-seconds 0 total-seconds-given?))
  "Display the amount of time this app has run for."
  (let* ((total-seconds (if total-seconds-given? total-seconds (get-run-time)))
         (days (floor (/ total-seconds 60 60 24)))
         (hours (- (floor (/ total-seconds 60 60))
                   (* days 24)))
         (minutes (- (floor (/ total-seconds 60))
                     (* days 24 60)
                     (* hours 60)))
         (seconds (- (floor total-seconds)
                     (* days 24 60 60)
                     (* hours 60 60)
                     (* minutes 60)))
         (millis (floor (* 1000 (nth-value 1 (floor total-seconds))))))
    (format t "~%~A (clock) runtime: " (if (empty? app-name) "App" app-name))
    (if (>= days 1)
      (format t "~:D day~:P " days))
    (format t "~2,'0D:~2,'0D:~2,'0D.~2,'0D.~%" hours minutes seconds millis)))
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

;;; Terminal -------------------------------------------------------------------
(defvar *SIGINT* 2)

(defmacro handle-signal (signo &body body)
  "Handle Unix signal.
   Note that `signo` must be a simple integer. I.e. it shouldn't even be a
   function that evaluates to an integer as this macro doesn't support this.
   Taken from https://stackoverflow.com/a/10442062/24318."
  (let ((handler (gensym "HANDLER")))
    `(progn
       (cffi:defcallback ,handler :void ((signo :int))
                         (declare (ignore signo))
                         ,@body)
       (cffi:foreign-funcall "signal"
                             :int ,signo
                             :pointer (cffi:callback ,handler)))))

(defstruct cmd-opts
  (name "")
  (short-name "")
  (description "")
  (default-value "")
  (arg-parser nil))

(defun parse-cmd-line-args (args opts)
  "TODO"
  (if (or (empty? args) (empty? opts))
    (return-from parse-cmd-line-args args))
  (let* ((i 0)
         (parsed '()))
    (dolist (arg args)
      ;; Skip arguments that don't start with a hyphen
      (if (starts-with "-" arg)
        (let* ((trimmed-arg (trim #\- arg))
               (matched-opt nil))
          (format t "Parsing arg ~A: ~A~%" i trimmed-arg)
          (setf matched-opt
                (find trimmed-arg opts
                      :key 'cmd-opts-name
                      :test #'string-equal))
          (if (empty? matched-opt)
            (push (new-r :error (sf "Unrecognised argument: ~A." arg))
                  parsed)
            (progn
              (format t "Found: ~A~%" matched-opt)
              (if (not (empty? (cmd-opts-arg-parser matched-opt)))
                (format t "has parser")
                )
              )
            )
          (push trimmed-arg parsed)))
      (incf i))
    parsed))
;;; Terminal -------------------------------------------------------------------
