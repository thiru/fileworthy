(ns fileworthy.core.users
  "Contains functionality around registered users."
  (:require
            [clojure.string :as str]
            [clojure.pprint :refer :all]

            [glu.ednfilestore :as efs]
            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.core :refer :all]))

(def file "data/users.edn")

(defn get-all
  "Get all users.

  Returns:
  * A map of users keyed by their unique username or,
  * `nil` if there are no registered users"
  []
  (let [all-users (efs/load-edn file)]
    (if (failed? all-users)
      (log :error (meta all-users)))
    all-users))

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

(defn is-admin?
  "Determine whether the given user is an administrator."
  [user]
  (and user (some #(case-insensitive= % "admin") (:roles user))))
