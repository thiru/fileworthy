;; ## Summary
;;
;; Contains generic, miscellaneous utilities.
;;
;; This namespace does not contain any domain-specific code.
;;
(ns thiru.utils
  (:require
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint]
            [clojure.string :as string]
            [java-time :as jt]

            [thiru.reporting :refer :all]))

(defn as-english-number
  "Print the given number in English words.

  E.g. `(as-english-number 23) ;=> \"Twenty-three\"`."
  [num]
  (clojure.pprint/cl-format nil "~@(~@[~R~]~^ ~A.~)" num))

(defn trim-version
  "Trims version strings of trailing segments containing only zeroes.

  E.g. `(trim-version \"0.1.0\") ;=> \"0.1\"`."
  [version]
  (string/replace version #"(\.0+)+$" ""))

(defn non-empty?
  "Simply the negation of `empty?`."
  [obj]
  (not (empty? obj)))

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

(defn load-config
  "Load an EDN config file from the given path and return it's contents.

  A result map is returned containing the config value.

  Note that a key named *updated* is added which holds the last modified time
  of the config file as a `java-time` instant object.

  * `path`
    * The path to the config file
  * `config-defaults`
    * A map specifying default/fallback values
    * This map will be merged with the loaded config (if any) in order to
      supply required defaults

  If the config file doesn't exist at the given path `nil` is returned."
  [path config-defaults]
  (let [file (io/as-file path)
        file-exists (.exists file)
        config (if file-exists
                 (or (-> path slurp edn/read-string)
                     ""))
        empty-cfg? (empty? config)]
    (if file-exists
      (r :success
         (str "Config found at '" path "'"
              (if empty-cfg? ", though it's empty" ""))
         (merge config-defaults
                {:updated (config-updated path)}
                config)
         {:empty? empty-cfg?})
      (r :debug
         (str "Config not found at '" path "'")
         (merge config-defaults config)
         {:empty? empty-cfg?}))))

(defn deps-for-marg
  "Loads *deps.edn* and generates a semi-colon delimited string of dependencies
  that can be consumed by the *marginalia* plugin.

  The general form is <group1>:<artifact1>:<version1>;<group2>...

  If the loading of *deps.edn* fails a result object is returned describing the
  error."
  []
  (let [deps-r (load-config "deps.edn" {})]
    (if (success? deps-r)
      (string/join ";"
                   (mapv #(str "mvn:" (first %) ":" (:mvn/version (second %)))
                         (-> deps-r :data :deps)))
      deps-r)))

(defn hostname
  "Get the computer's hostname."
  []
  (.. java.net.InetAddress getLocalHost getHostName))
