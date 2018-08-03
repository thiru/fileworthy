(defproject fileworthy "2.0-alpha"
  :description "A simple website to manage your notes and files across all your devices."
  :url "https://github.com/thiru/fileworthy"
  :license {:name "GPLv3"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :plugins [[lein-marginalia "0.9.1"]
            [lein-tools-deps "0.4.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]
                           :clojure-executables ["/usr/bin/clojure"]}
  :main ^:skip-aot user
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
