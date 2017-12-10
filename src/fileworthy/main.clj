;; ## Summary
;;
;; Command-line interface and entry-point into the application.
;;
;; Command-line arguments are parsed and displayed with the help of
;; [clojure.tools.cli](https://github.com/clojure/tools.cli).
;;
(ns fileworthy.main
  (:require [clojure.set :refer [difference]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [common.utils :refer :all]
            [fileworthy.app :as app]
            [fileworthy.web.server :as server])
  (:gen-class))

(def cli-commands
  "A map of CLI (sub-)commands with a short description of each."
  {:start "Start web server"
   :version "Print app version"})

(def cli-options
  "A vector of CLI options.
 
  Each item follows the spec of a CLI option as defined by
  `clojure.tools.cli`."
  [["-p" "--port PORT" "Web server listen port"
    :default (:port @app/config)
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Port must be a number between 0 and 65536"]]

   ["-l" "--log-level LEVEL"
    (str "Log verbosity level (" (level-names) ")")
    :default (:log-level @app/config)
    :default-desc (name (:log-level @app/config))
    :parse-fn #(first (find levels (keyword %)))
    :validate [#(get levels %)
               (str "Log verbosity level must be one of: " (level-names))]]

   ["-v" "--version" "Show app version"]

   ["-h" "--help" "Show help"]])

(defn usage [options-summary]
  "User-friendly CLI usage description.

  * `options-summary`
    *  A user-friendly _summary of CLI options_ to be injected into the full
       summary string returned
    *  The options summary is generated with `clojure.tools.cli/parsed-opts`"
  (->> [(str (:name app/info) " " (:version app/info))
        ""
        (:description app/info)
        ""
        (str "Usage: java -jar "
             (string/lower-case (:name app/info))
             ".jar"
             " [options] command")
        ""
        "Options:"
        options-summary
        ""
        "Commands:"
        (apply str
               (map (fn [kvp] (str "  " (name (key kvp)) " - " (val kvp) "\n"))
                    cli-commands))]
       (string/join \newline)
       (string/trim)))

(defn error-msg
  "Common error message format.
 
  * `errors`
    * A vector of error messages"
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  "Validate the given command-line arguments.
  A map is returned of the following form:

  * `:ok?`
    * Whether the validation was successful or not
  * `:exit-message`
    * A message to be printed on exiting the app
    * This could contain error messages and/or the usage summary
  * `:command`
    * A keyword specifying the command to run the app with
    * See `cli-commands`"
  [args]
  (let [parsed-opts (parse-opts args cli-options)
        {:keys [options arguments errors summary]} parsed-opts]
    (cond
      ;; Help option was specified
      (:help options)
      {:exit-message (usage summary) :ok? true}

      ;; Version option was specified
      (:version options)
      {:exit-message (:version app/info) :ok? true}

      ;; Errors were found while parsing
      errors
      {:exit-message (error-msg errors)}

      ;; Ensure only one command is given
      (>= (count arguments) 2)
      {:exit-message "Only one command at a time is supported"}

      ;; Ensure the given command is valid
      (and (= 1 (count arguments))
           (seq (difference
                  (set (map keyword arguments))
                  (set (keys cli-commands)))))
      {:exit-message (str "Unrecognised command: " (first arguments))}

      ;; Actual handling of the (sub-)commands
      (and (= 1 (count arguments))
           (get cli-commands (keyword (first arguments))))
      {:command (keyword (first arguments)) :options options}

      ;; Failed custom validation. Exit with usage summary.
      :else
      {:exit-message (usage summary)})))

(defn exit
  "Exit app with code `status` and print `msg` to standard out."
  [status msg]
  (println msg)
  (System/exit status))

(defn -main
  "This function is called when the **app first starts up**.
  
  * `args`
    * A vector of command-line arguments"
  [& args]
  (let [{:keys [command options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (with-redefs [log-level (:log-level options)]
        (log :debug (str "Log level set to '" (name log-level) "'"))
        (case command
          :start (server/start (:port options))
          :version (println (:version app/info)))))))
