;; ## Summary
;;
;; Common utilities used throughout the project.
;;
;; This namespace does not contain any domain-specific code, and so should be
;; easy to use in other projects.
;;
(ns common.utils
  (:require [clojure.string :as string]
            [clansi :refer :all]))

;; ## General

(def as-english-number (partial clojure.pprint/cl-format nil "~@(~@[~R~]~^ ~A.~)"))

(defn non-empty?
  "Simply the negation of `empty?`."
  [obj]
  (not (empty? obj)))

(defn has-more-items
  "Determine whether `items` has more items after the index `idx`."
  [idx items]
  (< (+ 1 idx) (count items)))

;; ## Reporting

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
    * An optional data item around which this result is wrapped"
  ([] (r :info))
  ([level] (r level ""))
  ([level msg] {:level level :message msg})
  ([level msg data] {:level level :message msg :data data}))

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

;; ## Logging

(def log-level
  "The current logging level, which should be a value from `levels`."
  :debug)

(defn loggable?
  "Determine whether given object should be logged.
  
  * `obj`
    * Can be any object
    * `nil` is not considered loggable
    * If this is a valid result map (see `is-result-map?`) we compare it's
      `:level` key to the current value of `log-level` to determine if it's
      loggable
    * All other (non-`nil`) objects are considered always loggable"
  [obj]
  (cond
    (nil? obj)
    false
    
    (is-result-map? obj)
    (let [r-level-num (if (number? (:level obj))
                        (:level obj)
                        (get levels (:level obj) -10))
          log-level-num (get levels log-level -10)]
      ;; Log everything if the current log-level is 0/debug
      (or (= :debug log-level)
          (and (pos? log-level-num)
               (or (>= r-level-num log-level-num)
                   (neg? r-level-num)))
          (and (neg? log-level-num)
               (<= r-level-num log-level-num))))
    
    :else
    true))

(defn log
  "Log to console.
  
  * `level`
    * Should be a number or keyword from `levels`
  * `obj`
    * Object to be printed
    * If the 2-arity function is specified it is assumed that this is **not** a
      result map, since a result map already has a `:level` key making the
      2-arity call redundant"
  ([level obj] (log {:level level :message obj}))
  ([obj]
   (cond
     (nil? obj)
     nil
    
     (and (is-result-map? obj))
     (if (loggable? obj)
      (let [r-level-num (if (number? (:level obj))
                          (:level obj)
                          (get levels (:level obj) -10))
            colour (cond
                     (>= r-level-num 2)
                     :green

                     (= r-level-num 1)
                     :bright

                     (= r-level-num 0)
                     :magenta
                    
                     (= r-level-num -1)
                     :yellow
                    
                     (<= r-level-num -2)
                     :red
                    
                     :else
                     :default)]
        (println (style (str (string/upper-case (name (:level obj)))
                             ": "
                             (:message obj))
                  colour))))

     :else
     (do
       (println obj)))))
