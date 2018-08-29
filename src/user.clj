(ns user
  "Initial namespace loaded when using a REPL (e.g. using `clj`)."
  (:require
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :refer :all]
            [clojure.reflect :as reflect]
            [clojure.repl :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as string]

            [cheshire.core :as json]
            [figwheel.main.api :as fig]

            [glu.core :refer :all]
            [glu.logging :refer :all]
            [glu.repl :as repl]
            [glu.results :refer :all]

            [fileworthy.app :as app]
            [fileworthy.main :as cli]
            [fileworthy.core.users :as users]
            [fileworthy.web.server :as server]))

(defn cljs
  "Quickly jump into the ClojureScript REPL."
  []
  (fig/cljs-repl "dev"))
