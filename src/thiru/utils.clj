;; ## Summary
;;
;; Common utilities used throughout the project.
;;
;; This namespace does not contain any domain-specific code, and so should be
;; easy to use in other projects.
;;
(ns thiru.utils
  (:require [clojure.string :as string]))

(def as-english-number (partial clojure.pprint/cl-format nil "~@(~@[~R~]~^ ~A.~)"))

(defn non-empty?
  "Simply the negation of `empty?`."
  [obj]
  (not (empty? obj)))

(defn has-more-items
  "Determine whether `items` has more items after the index `idx`."
  [idx items]
  (< (+ 1 idx) (count items)))

