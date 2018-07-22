(defproject fileworthy "2.0-alpha"
  :description "A media workflow management system"
  :dependencies
  [[org.clojure/clojure "1.9.0"]

   ;; Configuration loader
   [aero "1.1.3"]

   ;; ## Database

   ;; Date/time library:
   [clojure.java-time "0.3.1"]

   ;; Converts between different string case conventions:
   [camel-snake-kebab "0.4.0"]

   ;; ## Web Server

   ;; Web server abstraction based on middleware:
   [ring/ring-core "1.6.3"]
   ;; Development middleware (e.g. middleware to auto-reload handlers):
   [ring/ring-devel "1.6.3"]
   ;; In-process web server:
   [ring/ring-jetty-adapter "1.6.3"]
   ;; Sensible middleware defaults for Ring:
   [ring/ring-defaults "0.3.1"]
   ;; Simpler HTTP response handling (replaces `ring.util.response`):
   [metosin/ring-http-response "0.9.0"]
   ;; Routing library for Ring
   [compojure "1.6.0"]
   ;; Generate HTML from clojure data structures:
   [hiccup "1.0.5"]
   ;; JSON encoding/decoding:
   [cheshire "5.8.0"]
   ;; Better exception reporting middleware:
   [prone "1.1.4"]
   ;; Security library:
   [buddy "2.0.0"]

   ;; ## Logging & Debugging

   ;; Tracing tools during development:
   [org.clojure/tools.trace "0.7.9"]

   ;; The following libraries revolve around SLF4J (Simple Logging Facade for
   ;; Java). SLF4J serves as a simple facade or abstraction for various
   ;; logging frameworks (e.g. log4j, java.util.logging, logback, etc.),
   ;; allowing the user to plug in the desired logging framework at
   ;; *deployment* time.
   ;;
   ;; These are needed in order to get logs from Jetty.

   ;; SLF4J binding for Jakarta Commons Logging:
   [org.slf4j/jcl-over-slf4j "1.7.25"]
   ;; SLF4J binding for java.util.logging:
   [org.slf4j/jul-to-slf4j "1.7.25"]
   ;; SLF4J binding for log4j:
   [org.slf4j/log4j-over-slf4j "1.7.25"]
   ;; SLF4J native binding:
   [ch.qos.logback/logback-classic "1.2.3" :exclusions [org.slf4j/slf4j-api]]

   ;; ## Tooling

   ;; Clojure *network* REPL server/client:
   [org.clojure/tools.nrepl "0.2.13"]
   ;; nREPL middleware for CIDER. This is used by Vim's Fireplace plugin:
   [cider/cider-nrepl "0.16.0"]
   ;; Command-line interface helper:
   [org.clojure/tools.cli "0.3.5"]
   ;; ANSI colours for terminal:
   [clansi "1.0.0"]
   ;; Literate programming-ish documentation
   [lein-marginalia "0.9.1"]
   ;; Namespace change tracker:
   [ns-tracker "0.3.1"]
   ;; File-system watcher:
   [hawk "0.2.11"]]
  :plugins [[lein-marginalia "0.9.1"]]
  :main ^:skip-aot user
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
