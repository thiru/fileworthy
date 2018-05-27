;; ## Summary
;;
;; Contains functionality around registered users.
;;
(ns fileworthy.core.users
  (:require
            [clojure.string :as str]
            [clojure.pprint :refer :all]

            [glu.ednfilestore :as efs]
            [glu.logging :refer :all]
            [glu.reporting :refer :all]
            [glu.utils :refer :all]

            [fileworthy.app :as app]))

(def file "data/users.edn")

(defn get-all
  "Get all users.

  Returns:
  * A map of users keyed by their unique username or,
  * `nil` if there are no registered users"
  []
  (efs/load file))

(defn get-one
  "Get the first user matching the specified criteria.

  * `filters`
    * A map specifying filtering criteria
    * E.g. `{:username \"bob\"}`

  Returns:
  * A map specifying a user, if found
  * `nil` if there were no matches"

  [filters]

  (let [all-users (get-all)]
    (when (non-empty? all-users)
      (cond
        (:username filters)
        (get all-users (:username filters))))))
