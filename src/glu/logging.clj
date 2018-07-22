(ns glu.logging
  "Basic console logging facilities."
  (:require
            [clojure.spec.alpha :as s]
            [clojure.string :as string]

            [clansi :refer :all]

            [glu.core :refer :all]
            [glu.results :refer :all]))

(declare log log-r)

(defonce
  ^{:doc "The current logging level, which should be a value from `levels`."}
  log-level
  :debug)

(defn set-log-level
  [new-val]
  (if (not= log-level new-val)
    (alter-var-root #'log-level (constantly new-val)))
  (log :info (fmt "Log level set to ~A" (string/upper-case (name new-val)))))

(defn loggable-level?
  "Determine if the given level is currently loggable (based on `log-level`)."
  [level]
  ;; Log **everything** if the current log-level is *debug*
  (or (= :debug log-level)
      (let [given-level-as-num (if (number? level)
                                 level
                                 (get levels level -10))
            global-level-as-num (get levels log-level -10)]
        (or (and (pos? global-level-as-num)
                 (or (>= given-level-as-num global-level-as-num)
                     (neg? given-level-as-num)))
            (and (neg? global-level-as-num)
                 (<= given-level-as-num global-level-as-num))))))

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
    * Whether to print the logging level as a prefix to the log message

  Returns the given object."
  [level obj & {:keys [print-level-prefix]
                :or {print-level-prefix true}}]
  (when (loggable-level? level)
    (if print-level-prefix
      (print (style (str (string/upper-case (name level)))
                    (colour-for-level level))))
    (println ":" (style obj)))
  obj)

;; TODO: add spec to verify result is passed in
(defn log-r
  "Log the given result map.

   * `print-level-prefix`
     * Whether to print the logging level as a prefix to the log message

   Returns the given object."

  [result & {:keys [print-level-prefix]
             :or {print-level-prefix true}}]

  (when (loggable-level? (:level result))
    (if print-level-prefix
      (print (style (str (string/upper-case (name (:level result))))
                    (colour-for-level (:level result)))))
    (println ":" (style (:message result))))
  result)

(defmacro catch-and-log
  "Swallow all thrown exceptions and log any errors."
  [& body]
  `(try ~@body
        (catch Exception e# (log :error e#))))

(defmacro catch-sans-log
  "Completely swallow all thrown exceptions, without even logging."
  [& body]
  `(try ~@body
        (catch Exception e#)))
