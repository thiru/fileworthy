(ns fileworthy.web.utils
  (:require
            [clojure.string :as string]

            [fileworthy.web.state :refer [state]]))

(defn set-page-title [title]
  (let [site-name (-> @state :site-info :name)]
    (set! js/document.title
          (if (string/blank? title)
            site-name
            (str title " - " site-name)))))
