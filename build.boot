;; ## Boot Build Definition

;; See https://github.com/boot-clj/boot for details on using Boot.

;; ### Dependencies

;; Library dependencies are defined in it's own var since it'll be used in at
;; least two places. One for defining the Boot environment, and for dynamically
;; generating a (Leiningen compatible) project.clj file (see below).
;;
(def deps
  "Clojure library dependencies used by this project."
  '[;; #### Clojure Core
    [org.clojure/clojure "1.9.0"]

    ;; #### Core Libraries

    ;; Soon to be obsolete library for schema validation (replaced by Spec)
    ;; (test if needed)
    [prismatic/schema "1.1.6"]

    ;; Async programming/communication
    [org.clojure/core.async "0.3.465"]

    ;; Data format & libraries for conveying values between apps across langs
    ;; (not sure what it's being used for)
    ;; (test if needed)
    [com.cognitect/transit-clj "0.8.300"]

    ;; #### Web Server

    ;; Bidirectional web routing
    [bidi "2.1.1"]

    ;; Generate HTML from clojure data structures
    [hiccup "1.0.5"]

    ;; Web framework
    [yada "1.2.9" :exclusions [ring-swagger]]

    ;; Async communication (HTTP, TCP, UDP, etc.)
    [aleph "0.4.3"]

    ;; Swagger spec (web apps)
    [metosin/ring-swagger "0.24.0"]

    ;; #### Logging
    ;; (test which of these are actually needed)]

    [org.clojure/tools.logging "0.4.0"]
    [org.slf4j/jcl-over-slf4j "1.7.25"]
    [org.slf4j/jul-to-slf4j "1.7.25"]
    [org.slf4j/log4j-over-slf4j "1.7.25"]
    [ch.qos.logback/logback-classic "1.2.3" :exclusions [org.slf4j/slf4j-api]]

    ;; #### Tooling

    ;; Command-line interface helper
    [org.clojure/tools.cli "0.3.5"]

    ;; ANSI colours for terminal
    [clansi "1.0.0"]])

;; ### Boot Environment
;;
(set-env!
  :source-paths #{"src"}
  :asset-paths #{"assets"}
  :dependencies deps)

(require '[fileworthy.app :as app]
         '[clojure.string :as string])

;; Define project metadata, etc.
;;
(task-options!
  pom {:project (symbol (string/lower-case (:name app/info)))
       :version (:version app/info)
       :description (:description app/info)
       :license {"GNU General Public License Version 3"
                 "https://www.gnu.org/licenses/gpl-3.0.en.html"}}
  aot {:namespace '#{fileworthy.main}}
  jar {:main 'fileworthy.main
       :file (str (string/lower-case (:name app/info)) ".jar")}
  sift {:include #{#"\.jar$"}})

(require '[common.utils :refer :all]
         '[clojure.java.shell :as shell]
         '[fileworthy.main :as cli]
         '[fileworthy.web.server :as server])

(deftask docs
  "Generate (Literate Programming) documentation using Marginalia.
  The existing Boot plugin for Marginalia doesn't seem to work so what we're
  doing here is dynamically generating a Leiningen project.clj file which the
  original Marginalia plugin then uses (via `lein marg`)."
  []
  (log :info "Creating Leiningen project.clj...")
  (spit "project.clj"
        (format "(defproject asdbwebapi \"%s\"
                :description \"%s\"
                :dependencies %s
                :plugins [[lein-marginalia \"0.9.0\"]]
                :main ^:skip-aot asdbwebapi.main
                :target-path \"target/%%s\"
                :profiles {:uberjar {:aot :all}})\n"
                (:version app/info)
                (:description app/info)
                deps))
  (let [cmd "lein marg --file index.html"]
    (log :info (str "Running '" cmd "'..."))
    (shell/sh "sh" "-c" cmd)
    (log :success "Doc generation complete!")))

(deftask run
  "Run the project with default settings."
  []
  (comp
    (with-pass-thru _
      (cli/-main))))

(deftask build
  "Build project and generate stand-alone jar."
  []
  (comp
    (aot)
    (pom)
    (uber)
    (jar)
    (sift)
    (target)))

