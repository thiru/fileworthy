;;;; Package definitions

(in-package :cl-user)

(defpackage :glu
  (:use :cl :local-time)
  (:documentation "Global Lisp Utilities")
  (:export
    :*english-list*
    :labeled-time
    :1st
    :2nd
    :last1
    :blank?
    :join
    :trim
    :->
    :=>
    :loose-parse-int
    :sf
    :to-string
    :empty/*objects*
    :empty
    :empty=>
    :empty?
    :levels
    :r
    :r-level
    :r-message
    :r-data
    :new-r
    :succeeded?
    :failed?
    :*log-format-time*
    :logm
    ))

(defpackage :fileworthy
  (:use :cl :cl-who :glu :hunchentoot :local-time :split-sequence :uiop)
  (:documentation "A simple website to manage your *local* notes and files across all your devices")
  (:export
    :*app*
    :start-app
    :stop-app
    :restart-app
    ))
