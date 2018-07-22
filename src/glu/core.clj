(ns glu.core
  "Contains core/generic/miscellaneous utilities which I feel would be useful
   in most of my apps."
  (:require
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]

            [aero.core :as cfg]
            [java-time :as jt]

            [glu.results :refer :all]))

(def app-envs
  "Supported deployment environments for the app."
  #{:development :staging :production})

(def default-app-env :development)

(defonce
  ^{:doc "Contains the global app configuration map."}
  config
  (atom {}))

(def fmt
  "A short-form to using `cl-format` which returns a formatted string."
  (partial cl-format nil))

(defn as-english-number
  "Print the given number (digits) in English words.

  E.g. `(as-english-number 23) ;=> \"Twenty-three\"`."
  [num]
  (cl-format nil "~@(~@[~R~]~^ ~A.~)" num))

(defn non-empty?
  "Simply the negation of `empty?`."
  [obj]
  (not (empty? obj)))

(defn case-insensitive=
  "Determine whether the given strings are equal, ignoring case."
  [string1 string2]
  (if (and (= (count string1) (count string2))
           (.equalsIgnoreCase (or string1 "") (or string2 "")))
    string1))

(defn case-insensitive-starts-with?
  "Determine whether there's a case-insensitive sub-string match."
  [full-string sub-string]
  (if (string/starts-with? (string/lower-case full-string)
                          (string/lower-case sub-string))
    full-string))

(defn has-string?
  "Determine if there's a case-insensitive match.

   * `coll`
     * The sequence of strings to search against
   * `item`
     * The string to search for
   * `test-fn`
     * An optional function to perform the 'has' test
     * By default this uses `case-insensitive=` but you may want to use
       `case-insensitive-starts-with?` instead, for example

   Returns the match, if any."
  [coll item & {:keys [test-fn]}]
  (let [test-fn (or test-fn case-insensitive=)]
    (some #(apply test-fn [% item]) coll)))

(defn map-keys-cds
  "Get the names of the map keys as a comma-delimited string."
  [m]
  (string/join ", " (map name (keys m))))

(defn set-as-cds
  "Get a comma-delimited string of the items in the given set."
  [s]
  (string/join ", " (map name app-envs)))

(defn file-exists?
  "Determine whether the file exists.

  * `file-path`
    * A string specifying the file path

  Returns:
  * A `java.io.File` if the file exists
  * `false` if the file doesn't exist/was not found"

  [file-path]

  (let [file (io/as-file file-path)]
    (if file
      (.exists file))))

(defn config-updated
  "Get the last modified time of the specified config file.

   * `path`
     * Specifies the path to the config file
     * If this is blank, *config.edn* is used

   Returns a `java-time` instant object if successful, otherwise nil."
  ([] (config-updated "config.edn"))
  ([path]
   (let [path (if (string/blank? path) "config.edn" path)
         file (io/as-file path)
         file-exists (.exists file)]
     (if file-exists
       (jt/instant (.lastModified file))))))

(defn load-config!
  "Load/reload config file.

  * `profile`
    * The profile in which to load the config
    * This should be a key from `app-envs`
    * See `aero.core` for more info
  * `path`
    * An optional path to the config file"
  [& {:keys [profile path]
      :or {profile default-app-env
           path "config.edn"}}]
  (let [cfg (merge (cfg/read-config path {:profile profile})
                   {:environment profile
                    :updated (config-updated path)})]
    (reset! config cfg)
    cfg))

(defmacro condv
  "Behaves just like `cond`, while also printing out the condition that was
  chosen. Use this while debugging/testing to easily determine which branch
  was taken in a `cond`.

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

(defn hostname
  "Get the computer's hostname."
  []
  (.. java.net.InetAddress getLocalHost getHostName))
