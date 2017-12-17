;; ## Summary
;;
;; Debugging and logging functionality.
;;
;; This namespace does not contain any domain-specific code, and so should be
;; easy to use in other projects.
;;
(ns thiru.debugnlog
  (:require [clojure.string :as string]
            [clansi :refer :all]
            [thiru.reporting :refer :all]))

(defmacro condv
  "Behaves just like `cond`, while also printing out the condition that was
  chosen.
  
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
