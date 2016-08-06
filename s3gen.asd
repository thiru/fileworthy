;;;; Build configuration

(in-package :cl-user)

(asdf:defsystem :s3gen
  :version (:read-file-form "version")
  :description "A super-simple website generator"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (:alexandria :cl-json :cl-who :glu :hunchentoot :ironclad
               :local-time :split-sequence)
  :components ((:file "app")))
