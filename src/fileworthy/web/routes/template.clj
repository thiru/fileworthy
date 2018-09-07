(ns fileworthy.web.routes.template
  "Common template page for all pages."
  (:require
            [clojure.string :as string]
            [clojure.pprint :refer :all]

            [hiccup.core :refer :all]
            [hiccup.page :refer :all]

            [glu.core :refer :all]
            [glu.logging :refer :all]
            [glu.results :refer :all]

            [fileworthy.core.users :as users]))

(defn template-page
  "Common template used by all pages on the site.

  * `req`
    * The ring request map
  * `title`
    * The title of the page
  * `page-id`
    * A unique id for the page
  * `content`
    * Hiccup structure containing the body of the page
  * `user`
    * The currently logged in user (if any)
  * `css-files`
    * An optional list of CSS files to include
  * `script-files`
    * An optional list of Javascript files to include"
  [req title page-id content & {:keys [user css-files script-files]}]
  (let [user (if (empty? user)
               (users/get-one {:username (-> req :session :username)})
               user)]
    (html5 {:lang "en"}
      [:head

       ;; Meta
       [:meta {:charset "utf-8"}]
       [:meta {:http-equiv "X-UA-Compatible"
               :content "IE=edge"}]
       [:meta {:name "robots" :content "noindex, nofollow"}]
       [:meta {:name "theme-color" :content "#0F83BC"}]
       [:meta {:name "viewport"
               :content "width=device-width, initial-scale=1"}]

       ;; Title
       [:title (if (string/blank? (:site-name @config))
                 title
                 (str title " - " (:site-name @config)))]

       ;; Manifest (for smart phone icon)
       [:link {:href "/manifest.json" :rel "manifest"}]

       ;; Fav Icon
       [:link {:href (str "/images/logo.png?v=" :version @config)
               :rel "icon"
               :type "image/png"}]

       ;; CSS (dependencies)
       [:link {:href "/deps/fira/fira.css" :rel "stylesheet"}]
       [:link {:href "/deps/font-awesome/css/all.css" :rel "stylesheet"}]
       [:link {:href "/deps/highlightjs/styles/monokai.css" :rel "stylesheet"}]

       ;; CSS (global domain)
       [:link {:href (str "/css/main.css?v=" (:version @config))
               :rel "stylesheet"}]
       [:link {:href (str "/css/main.mobile.css?v=" (:version @config))
               :rel "stylesheet"}]
       [:link {:href (str "/css/main.mobile.css?v=" (:version @config))
               :rel "stylesheet"}]

       ;; CSS (page-specific domain)
       (if (non-empty? css-files)
         (for [cf css-files]
           [:link {:href (str "/css/" cf "?v=" (:version @config))
                   :rel "stylesheet"}]))

       ;; Scripts (external dependencies)
       [:script {:src "/deps/lodash/lodash.min.js"}]
       [:script {:src "/deps/momentjs/moment.min.js"}]
       [:script {:src "/deps/jquery/jquery-2.1.3.min.js"}]
       [:script {:src "/deps/rxjs/rx.all.min.js"}]
       [:script {:src "/deps/markedjs/marked.min.js"}]

       ;; This global variable contains general info about the page/website.
       ;; It's stored as and EDN string so we can leverage it easily from
       ;; ClojureScript.
       [:script (fmt "fwSiteInfo = '~A'"
                     (pr-str
                       {:site-info {:site-name (:site-name @config)
                                    :description (:description @config)
                                    :version (:version @config)
                                    :updated (.toString (:updated @config))}
                        :user (select-keys user [:name :roles :username])}))]

       ;; Scripts (domain-specific)
       [:script {:src (str "/js/utils.js?v=" (:version @config))}]
       [:script {:src (str "/js/main.js?v=" (:version @config))}]
       (if (non-empty? script-files)
         (for [sf script-files]
           [:script {:src (str "/js/" sf "?v=" (:version @config))}]))]

      [:body
        [:div#app
          ;; Used for modal dialogs
          [:div#overlay.hidden]

          ;; Page content
          [:main #_ content]]

        ;; ClojureScript
        [:script {:src "/cljs/main.js"}]])))
