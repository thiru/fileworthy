(ns fileworthy.utils)

(def as-english-number (partial clojure.pprint/cl-format nil "~@(~@[~R~]~^ ~A.~)"))
