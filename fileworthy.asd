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
  :components ((:file "src/packages")
               (:file "src/glu")
               (:file "src/core/config")
               (:file "src/core/app")
               (:file "src/core/user")
               (:file "src/core/utils")
               (:file "src/web/utils")
               (:file "src/web/routes")
               (:file "src/web/page-template")
               (:file "src/web/page-errors")
               (:file "src/web/page-about")
               (:file "src/web/page-settings")
               (:file "src/web/page-user-list")
               (:file "src/web/page-user-detail")
               (:file "src/web/page-user-save")
               (:file "src/web/page-login")
               (:file "src/web/page-logout")
               (:file "src/web/page-fs-search")
               (:file "src/web/page-fs-path")
               (:file "src/main")))
