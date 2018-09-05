(ns fileworthy.web.utils
  (:require
            [clojure.string :as string]
            [cljs.reader :as reader]))

(defn parse-site-info
  "Parse an embedded EDN string containing general info about the page/website.

   This includes things like the site configuration, the currently logged in
   user, etc."
  []
  (reader/read-string js/fwSiteInfo))
