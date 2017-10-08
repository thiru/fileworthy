(defproject fileworthy "0.1.0-SNAPSHOT"
  :description "A simple website to manage your *notes* and files across all your devices"
  :url "https://github.com/thiru/fileworthy"
  :license {:name "GNU General Public License Version 3"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot fileworthy.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
