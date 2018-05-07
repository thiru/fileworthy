;; ## Summary
;;
;; General facilities around reporting and validation.
;;
;; This namespace does not contain any domain-specific code, and so should be
;; easy to use in other projects.
;;
(ns thiru.reporting
  (:require [clojure.string :as string]))

(def app-modes
  "The supported modes in which an app runs."
  #{:dev :test :prod})

(def levels
  "A generic map of levels that can be used for logging, reporting, etc.

  Negative values represent some level of failure. Non-negative values indicate
  some level of success, or just plain reporting."
  {:success 2
   :info 1
   :debug 0
   :warning -1
   :error -2
   :fatal -3})

(defn level-names
  "A comma-delimited string of `level` names."
  []
  (string/join ", " (map name (keys levels))))

(defn r
  "A map representing the result of some operation, including the level of
  success/failure, and an optional data item.

  * `level`
    * A value specifying the success/failure level
    * This should either be a key from `levels` (which map to numbers) or a
      number
    * By convention, negative values are considered a level of failure, while
      non-negative values are considered informational or successful
  * `msg`
    * A message describing the result
  * `data`
    * An optional data item around which this result is wrapped
  * `more`
    * An optional set of key/value pairs to include in the result map"
  ([] (r :info))
  ([level] (r level ""))
  ([level msg] {:level level :message msg})
  ([level msg data] {:level level :message msg :data data})
  ([level msg data more]
   (merge
     {:level level :message msg :data data}
     more)))

(defn is-result-map?
  "Determine whether `obj` is a valid \"result\" map.

  `obj` is considered a valid result map if it contains a key named `:level`
  that is a number or keyword."
  [obj]
  (and (map? obj)
       (or (keyword? (:level obj))
           (number? (:level obj)))))

(defn success?
  "Determine whether the given object represents a successful outcome.

  `obj` is considered successful in all cases except the following:

  * `nil`
  * `false`
  * A \"result\" map with a key named `:level` where
    * `:level` is a keyword defined in `levels` which maps to a negative number
    * `:level` is a negative number"
  [obj]
  (cond
    (nil? obj)
    false

    (false? obj)
    false

    (map? obj)
    (let [level (:level obj)]
      (cond
        (number? level)
        (<= 0 level)

        (keyword? level)
        (<= 0 (get levels level (:error levels)))))

    :else
    true))

(defn failed?
  "Determine whether the given object represents a failure outcome.

  This is basically the opposite of `success?`."
  [obj]
  (not (success? obj)))

