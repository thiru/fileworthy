(ns fileworthy.main
  "Command-line interface and entry-point into the application.

   Command-line arguments are parsed and displayed with the help of
   [clojure.tools.cli](https://github.com/clojure/tools.cli)."
  (:require
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]

            [rebel-readline.main :as rebel]

            [glu.logging :refer :all]
            [glu.results :refer :all]
            [glu.repl :as repl]
            [glu.core :refer :all]

            [fileworthy.app :as app]
            [fileworthy.web.server :as server])
  (:gen-class))

(def startup-command
  "Holds the command the app was started with (intended for debugging)."
  nil)

(def startup-options
  "Contains the command-line options the app was started with (intended for
   debugging)."
  nil)

(def cli-commands
  "A map of CLI commands with a short description of each."
  {:start "Start web server"
   :repl "Start interactive REPL"
   :version "Print app version"})

(def env-opt
  "Specification for the application environment CLI option (used by
   `clojure.tools.cli`).

   This CLI option is separated from the other options (see `cli-options`)
   because it needs to parsed first and independently. The reason for this is
   that some of the other options need to know the config defaults, which isn't
   known until the application environment is determined.  After this, the
   remaining options can be parsed and handled appropriately."

  ["-e" "--environment ENVIRONMENT"
   (fmt "Environment in which the application is running (~A)"
        (set-as-cds app-envs))
   :default default-app-env
   :default-desc (name default-app-env)
   :parse-fn #(keyword
                (has-string?
                  (map name app-envs)
                  %
                  :test-fn case-insensitive-starts-with?))
   :validate [#(contains? app-envs %)
              (str "Environment must be one of: " (set-as-cds app-envs))]])

(defn cli-options
  "Get a vector of CLI options.

   Each item follows the spec of a CLI option as defined by
   `clojure.tools.cli`."

  [config]

  (vec
    (concat
      [env-opt]
      [["-p" "--web-server-port PORT" "Web server listen port"
        :default (:web-server-port config)
        :parse-fn #(Integer/parseInt %)
        :validate [#(< 0 % 0x10000)
                   "Port must be a number between 0 and 65536"]]

       ["-r" "--nrepl-port PORT" "nREPL server listen port"
        :default (:nrepl-port config)
        :parse-fn #(Integer/parseInt %)]

       ["-l" "--log-level LEVEL"
        (str "Log verbosity level (" (map-keys-cds levels) ")")
        :default (:log-level config)
        :default-desc (name (:log-level config))
        :parse-fn #(first (find levels (keyword %)))
        :validate [#(get levels %)
                   (str "Log verbosity level must be one of: "
                        (map-keys-cds levels))]]

       ["-v" "--version" "Show app version"]

       ["-h" "--help" "Show help"]])))

(defn usage
  "User-friendly CLI usage description.

  * `options-summary`
    *  A user-friendly summary of CLI options to be injected into the full
       summary string returned
    *  We generate the options summary with `clojure.tools.cli/parsed-opts`
  * `config`
    * The app config map"
  [options-summary config]
  (->> [(str "fileworthy " (:version config))
        ""
        (:description config)
        ""
        "Usage: fileworthy [options] command"
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

(defn validate-cli-args
  "Validate the given command-line arguments.

   * `args`
     * The list of command-line args provided by the user
   * `config`
     * The app config map

   Returns a result map with the following additional kvps:

   * `:command`
     * A keyword specifying the command to run the app with (if any)
     * See `cli-commands` for supported commands
   * `:options`
     * A map of effective/parsed options
     * See `cli-options` for a specfication of supported options"

  [args config]

  (let [parsed-opts (cli/parse-opts args (cli-options config))
        {:keys [options arguments errors summary]} parsed-opts]
    (cond
      ;; No arguments were given
      (empty? args)
      (r :warning
         (fmt "No command or options were specified. Showing help:~%~%~A"
              (usage summary config))
         :options options)

      ;; Help option was specified
      (:help options)
      (r :info
         (usage summary config)
         :options options)

      ;; Version option was specified
      (:version options)
      (r :info
         (:version config)
         :options options)

      ;; Errors were found while parsing
      errors
      (r :error
         (string/join \newline errors)
         :options options)

      ;; Ensure no more than one command is given
      (< 1 (count arguments))
      (r :warning
         (fmt "Only one command at a time is supported. Commands supplied: ~A"
              arguments)
         :options options)

      ;; Ensure the given command is valid (if any)
      (and (non-empty? arguments)
           (not-empty (sets/difference (set (map keyword arguments))
                                       (set (keys cli-commands)))))
      (r :error
         (fmt "Unrecognised command: ~A" (first arguments))
         :options options)

      ;; Handle commands
      (get cli-commands (keyword (first arguments)))
      (r :success
         ""
         :command (keyword (first arguments))
         :options options)

      ;; No command was specified, so show help
      :else
      (r :warning
         (fmt "No command was specified. Showing help:~%~%~A"
              (usage summary config))
         :options options))))

(defn parse-app-env
  "Parse and verify the application environment option.

   * `args`
     * A list of command-line arguments provided by the user

   Returns a result map with the parsed environment keyword (a value from
   `app-envs`) under the `:val` key."
  [args]
  (let [args (vec args)
        matched-indeces (keep-indexed
                          (fn [idx val]
                            (if (has-string? ["--environment" "-e"] val)
                              idx))
                          args)
        env-opt-idx (last matched-indeces)
        env-opt-specified? (and env-opt-idx
                                (<= 0 (last matched-indeces))
                                (< env-opt-idx (count args)))
        env-opt-args (if env-opt-specified?
                       (subvec args env-opt-idx (+ 2 env-opt-idx)))
        parsed-opt (if env-opt-specified?
                     (cli/parse-opts env-opt-args [env-opt]))]
    (cond
      (not env-opt-specified?)
      (r :success
         "User did not specify --environment option"
         :val default-app-env)

      ;; This shouldn't be possible but let's protect against it anyway
      (empty? parsed-opt)
      (throw (ex-info (str "Unexpected error parsing the --environment "
                           "option. The returned map (from `cli/parse-opts` "
                           "was empty.")
                      {:args args}))

      ;; Errors were found while parsing
      (:errors parsed-opt)
      (r :error
         (string/join \newline (:errors parsed-opt))
         :val default-app-env)

      :else
      (r :success
         "Found user-supplied --environment option"
         :val (-> parsed-opt :options :environment)))))

(defn exit
  "Exit app with the given result map, printing the message to standard out."
  [result]
  (if (success? result)
    (println (:message result))
    (log-r result :print-level-prefix? false))
  (System/exit
    (cond
      (failed? result) 1
      :else 0)))

(defn -main
  "This is the entry-point into the application (e.g. when run from the
   command-line.

   * `args`
     * A list of command-line arguments provided by the user
     * Each argument is a string"

  [& args]

  (let [app-env-r (parse-app-env args)]

    (if (failed? app-env-r)
      (exit app-env-r))

    (load-config! :profile (:val app-env-r))

    (let [parsed-cli-r (validate-cli-args args @config)]

      (alter-var-root #'startup-command (constantly (:command parsed-cli-r)))
      (alter-var-root #'startup-options (constantly (:options parsed-cli-r)))

      (swap! config merge (:options parsed-cli-r))

      (set-log-level (:log-level @config))

      (log :info (fmt "Application environment set to ~:@(~A~)"
                      (name (:val app-env-r))))
      (log :debug (fmt "User-provided CLI arguments: ~A" args))
      (log :debug (fmt "Effective CLI command: ~A" (:command parsed-cli-r)))
      (log :debug (fmt "Effective CLI options: ~A" (:options parsed-cli-r)))
      (log :info (fmt "Config: ~A" @config))

      (if (or (failed? parsed-cli-r) (nil? (:command parsed-cli-r)))
        (exit parsed-cli-r)
        (case (:command parsed-cli-r)
          :start (app/start :environment (:environment @config))
          :repl (do (app/start :environment (:environment @config))
                    (rebel/-main)
                    (repl/stop!)
                    (System/exit 0))
          :version (do (println (:version @config))
                       (System/exit 0)))))))
