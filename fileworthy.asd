;;;; Build configuration

(in-package :cl-user)

(asdf:defsystem :fileworthy
  :version (:read-file-form "version")
  :description "A simple website to manage your *local* notes and files across many devices"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (:alexandria :cl-json :cl-ppcre :cl-who :hunchentoot :ironclad
               :local-time :split-sequence :uiop)
  :components ((:file "package")
               (:file "glu")
               (:file "app")))
