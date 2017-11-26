;; ## Boot Build Definition

;; See https://github.com/boot-clj/boot for details on using Boot.

;; Clojure library dependencies for this project are defined in it's own var
;; since it'll be used in at least two places. One for defining the Boot
;; environment, and for dynamically generating a (Leiningen compatible)
;; project.clj file (see below).
;;
(def deps
  "Clojure library dependencies used by this project."
  '[[org.clojure/clojure "1.8.0"]
    ;; Command-line interface helper
    [org.clojure/tools.cli "0.3.5"]

    ;; ANSI colours for terminal
    [clansi "1.0.0"]

    ;; Web server back-end
    [io.pedestal/pedestal.service "0.5.3"]
    [io.pedestal/pedestal.route "0.5.3"]
    [io.pedestal/pedestal.jetty "0.5.3"]
    [org.slf4j/slf4j-simple "1.7.22"]])

;; Establish Boot environment.
;;
(set-env!
  :resource-paths #{"src"}
  :dependencies deps)

;; We need to refer to app info/metadata to specify task properties.
;;
(require '[fileworthy.app :as app])

;; Define project metadata, etc.
;;
(task-options!
  pom {:project 'fileworthy
       :version (:version app/info)
       :description (:description app/info)
       :license {"GNU General Public License Version 3"
                 "https://www.gnu.org/licenses/gpl-3.0.en.html"}}
  aot {:namespace '#{fileworthy.main}}
  jar {:main 'fileworthy.main}
  sift {:include #{#"\.jar$"}})

;; Require main project namespaces so they're easily accessible from the REPL.
;;
(require '[common.utils :refer :all]
         '[fileworthy.main :as cli]
         '[fileworthy.web.server :as server]
         '[fileworthy.web.service :as service]
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

