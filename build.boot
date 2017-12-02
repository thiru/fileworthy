;; ## Boot Build Definition

;; See https://github.com/boot-clj/boot for details on using Boot.

;; ### Dependencies

;; Library dependencies are defined in it's own var since it'll be used in at
;; least two places. One for defining the Boot environment, and for dynamically
;; generating a (Leiningen compatible) project.clj file (see below).
;;
(def deps
  "Clojure library dependencies used by this project."
  '[;; #### Clojure Languages
    [org.clojure/clojure "1.9.0-RC2"]
    [org.clojure/clojurescript "1.9.946"]

    ;; #### Tooling

    ;; NREPL tooling
    ;; (not sure what it's being used for)
    ;; (test if needed)
    [org.clojure/tools.nrepl "0.2.13"]

    ;; Needed for start-repl in cljs repl
    [com.cemerick/piggieback "0.2.2" :scope "test"]

    ;; Boot task to compile cljs apps
    [adzerk/boot-cljs "2.0.0" :scope "test"]

    ;; Boot task providing a REPL for cljs dev
    [adzerk/boot-cljs-repl "0.3.3" :scope "test"]

    ;; Boot task providing live-reload of browser css, images, etc.
    [adzerk/boot-reload "0.5.1" :scope "test"]

    ;; Cljs browser REPL using WebSockets
    [weasel "0.7.0" :scope "test"]

    ;; Boot task to compile Sass
    [deraen/boot-sass "0.3.1" :scope "test"]

    ;; Command-line interface helper
    [org.clojure/tools.cli "0.3.5"]

    ;; ANSI colours for terminal
    [clansi "1.0.0"]

    ;; #### General/Language

    ;; Namespace management
    ;; (not sure what it's being used for)
    ;; (test if needed)
    [org.clojure/tools.namespace "0.2.11"]

    ;; Soon to be obsolete library for schema validation (replaced by Spec)
    ;; (test if needed)
    [prismatic/schema "1.1.6"]

    ;; Async programming/communication
    [org.clojure/core.async "0.3.465"]

    ;; Data format & libraries for conveying values between apps across langs
    ;; (not sure what it's being used for)
    ;; (test if needed)
    [com.cognitect/transit-clj "0.8.300"]

    ;; #### Logging
    ;; (test which of these are actually needed)]

    [org.clojure/tools.logging "0.4.0"]
    [org.slf4j/jcl-over-slf4j "1.7.25"]
    [org.slf4j/jul-to-slf4j "1.7.25"]
    [org.slf4j/log4j-over-slf4j "1.7.25"]
    [ch.qos.logback/logback-classic "1.2.3" :exclusions [org.slf4j/slf4j-api]]

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

    ;; #### Web Client

    ;; Cljs interface to React.js
    [reagent "0.7.0"]])

;; ### Boot Environment
;;
(set-env!
  :source-paths #{"src"}
  :resource-paths #{"resources"}
  :asset-paths #{"assets"}
  :dependencies deps)

;; We need to refer to app info/metadata to specify task properties.
;;
(require '[fileworthy.app :as app]
         '[clojure.string :as string])

;; These were defined in the Yada project template
;; (test if all are needed)
(require '[adzerk.boot-cljs :refer [cljs]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
         '[adzerk.boot-reload :refer [reload]]
         '[deraen.boot-sass :refer [sass]]
         'clojure.tools.namespace.repl)

(def repl-port 5600)

;; Define project metadata, etc.
;;
(task-options!
  repl {:client true
        :port repl-port}
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
         '[fileworthy.main :as cli]
         '[clojure.java.shell :as shell])

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

(deftask dev
  "This is the main development entry point."
  []
  (set-env! :source-paths #(conj % "dev"))

  ;; Needed by tools.namespace to know where the source files are
  (apply clojure.tools.namespace.repl/set-refresh-dirs (get-env :directories))

  (comp
   (watch)
   (speak)
   (sass :output-style :expanded)
   (reload :on-jsload 'fileworthy.main/init)
   ; this is also the server repl!
   (cljs-repl :nrepl-opts {:client false
                           :port repl-port
                           :init-ns 'user})
   (cljs :optimizations :none)
   (target)))

(deftask static
  "This is used for creating optimized static resources under static"
  []
  (comp
   (sass :output-style :compressed)
   (cljs :optimizations :advanced)))

(deftask build
  []
  (comp
   (static)
   (target :dir #{"static"})))

(deftask run
  []
  (comp
   (repl :server true
         :port repl-port)
   (wait)))

(deftask uberjar
  "Build an uberjar"
  []
  (println "Building uberjar")
  (comp
   (static)
   (aot)
   (pom)
   (uber)
   (jar)
   (target)))

(deftask run
  "Run the project with default settings."
  []
  (comp
    (with-pass-thru _
      (cli/-main))))

