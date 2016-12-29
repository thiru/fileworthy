(in-package :cl-user)

(defpackage :fileworthy
  (:use :cl :cl-markup :ningle :local-time :split-sequence :uiop)
  (:documentation "A simple website to manage your *local* notes and files across all your devices")
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:export :*app* :start :stop :restart-app))
