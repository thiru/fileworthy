(in-package :cl-user)

(defpackage :fileworthy
  (:use :cl :cl-markup :hunchentoot :local-time :split-sequence :uiop)
  (:documentation "A simple website to manage your *local* notes and files across all your devices")
  (:export
    :*app*
    :start-app
    :stop-app
    :restart-app))
