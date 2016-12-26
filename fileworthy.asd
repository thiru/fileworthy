;;;; Build configuration

(in-package :cl-user)

(asdf:defsystem :fileworthy
  :version (:read-file-form "version")
  :description "A simple website to manage your *local* notes and files across many devices"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (:alexandria :clack :cl-markup :cl-ppcre :local-time :ningle
               :split-sequence :uiop)
  :components ((:file "glu")
               (:file "app")))
