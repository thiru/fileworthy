;;;; Build configuration

(in-package :cl-user)

(asdf:defsystem :fileworthy
  :version (:read-file-form "version")
  :description "A simple file-system-based website"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (:alexandria :clack :cl-markup :cl-ppcre :glu :local-time :ningle
               :split-sequence :uiop)
  :components ((:file "app")))
