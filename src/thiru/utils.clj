;; ## Summary
;;
;; Common utilities used throughout the project.
;;
;; This namespace does not contain any domain-specific code, and so should be
;; easy to use in other projects.
;;
(ns thiru.utils)

(def as-english-number (partial clojure.pprint/cl-format nil "~@(~@[~R~]~^ ~A.~)"))

