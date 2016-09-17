;;;; Build configuration

(in-package :cl-user)

(asdf:defsystem :fileworthy
  :version (:read-file-form "version")
  :description "A simple file-system-based website"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (:alexandria :cl-json :cl-who :glu :hunchentoot :ironclad
               :local-time :split-sequence :uiop)
  :components ((:file "app")))
