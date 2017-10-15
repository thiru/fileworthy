(set-env!
  :resource-paths #{"src"}
  :dependencies '[[org.clojure/clojure "1.8.0"]
                  [io.pedestal/pedestal.service "0.5.1"]
                  [io.pedestal/pedestal.route "0.5.1"]
                  [io.pedestal/pedestal.jetty "0.5.1"]
                  [org.slf4j/slf4j-simple "1.7.21"]])

(task-options!
  pom {:project 'fileworthy
       :version "0.22.0"
       :description "A simple website to manage your notes and files across all your devices"
       :license {"GNU General Public License Version 3"
                 "https://www.gnu.org/licenses/gpl-3.0.en.html"}}
  aot {:namespace '#{fileworthy.main}}
  jar {:main 'fileworthy.main}
  sift {:include #{#"\.jar$"}})

(require 'thiru.utils
         'fileworthy.main)

(deftask run []
  (comp
    (with-pass-thru _
      (fileworthy.main/-main))))

(deftask build []
  (comp
    (aot)
    (pom)
    (uber)
    (jar)
    (sift)
    (target)))

