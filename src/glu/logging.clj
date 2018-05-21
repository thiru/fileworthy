;; ## Summary
;;
;; Simple logging facilities.
;;
;; This namespace does not contain any domain-specific code, and so should be
;; easy to use in other projects.
;;
(ns glu.logging
  (:require [clojure.string :as string]
            [clansi :refer :all]
            [glu.utils :refer :all]
            [glu.reporting :refer :all]))

(defmacro condv
  "Behaves just like `cond`, while also printing out the condition that was
  chosen. Use this to while debugging/testing to easily determine which branch
  in a `cond` was taken.

  This was taken from [Eli Bendersky's website](https://eli.thegreenplace.net/2017/notes-on-debugging-clojure-code/)."
  [& clauses]
  (when clauses
    (list
     'if
     (first clauses)
     (if (next clauses)
       `(do (println (str "condv " '~(first clauses)))
            ~(second clauses))
       (throw (IllegalArgumentException.
               "cond requires an even number of forms")))
     (cons 'condv (next (next clauses))))))

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

(defn colour-for-level
  "Get the console colour to use for the given `level`."
  [level]
  (let [level-num (if (number? level)
                    level
                    (get levels level -10))]
    (cond
      (>= level-num 2)
      :green

      (= level-num 1)
      :bright

      (= level-num 0)
      :magenta

      (= level-num -1)
      :yellow

      (<= level-num -2)
      :red

      :else
      :default)))

(defn log
  "Log to console.

  * `level`
    * Should be a number or keyword from `levels`
  * `obj`
    * Object to be printed
  * `print-level-prefix`
    * Whether to print the logging level as a prefix to the log message"
  [level obj & {:keys [print-level-prefix]
                :or {print-level-prefix true}}]
  (when (loggable? obj)
    (let [obj-is-result-map (is-result-map? obj)
          obj-to-print (if obj-is-result-map
                         (:message obj)
                         obj)
          effective-level (if (and obj-is-result-map (nil? level))
                            (:level obj)
                            (or level :info))
          colour (colour-for-level effective-level)]
      (if print-level-prefix
        (print (style (str (string/upper-case (name effective-level)))
                      colour)))
      (println ":" (style obj-to-print))))
  obj)
