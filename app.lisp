(in-package :cl-user)

(defpackage :s3gen
  (:use :alexandria :cl :glu :split-sequence)
  (:documentation "The sole package for this app.")
  (:export :version :updated
           :start))

(in-package :s3gen)

(defparameter version
  (asdf::read-file-form (asdf:system-relative-pathname :s3gen "version")))

(defun start ()
  "Starts the app."
  (format t "s3gen v~A started~%" version))
