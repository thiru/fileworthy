(ns fileworthy.web.apis.site-info
  "General info about the website."
  (:require
            [clojure.string :as str]
            [clojure.pprint :refer :all]

            [ring.util.http-response :as hr]

            [glu.core :refer :all]

            [fileworthy.core.users :as users]))

(defn get-site-info-api
  "Gets general info about the website."
  [req]
  (let [user (users/get-one {:username (-> req :session :username)})]
    (-> {:site-info {:site-name (:site-name @config)
                     :description (:description @config)
                     :version (:version @config)
                     :updated (.toString (:updated @config))}
         :user (select-keys user [:name :roles :username])}
        hr/ok
        (hr/content-type "application/edn"))))
