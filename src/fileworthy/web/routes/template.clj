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

(defn gen-main-id
  "Generate a safe string to be used as the `main` HTML element's id.

  The primary intention is to use the id to target page-specific CSS styles."
  [page-title]
  (-> page-title
      (string/replace #"[^\w\d\s']+" "-")
      (string/lower-case)
      (str "-page")))

(defn build-top-header
  "Build HTML fragment for site-wide top header."
  [user]
  (html
    [:header#top-bar
      ;; Info/Settings Icon
      [:a {:href "/settings" :title "Info/Settings"}
        [:i.fas.fa-bars]]
      [:span " "]
      ;; Site Name
      [:a#app-name {:href "/" :title "Home"}
        (:site-name @config)]
      ;; User Info
      [:div#user-info
        (if (empty? user)
          ;; Logged Out
          [:a {:href "/login"}
            [:i.fas.fa-sign-in-alt]
            " Log In"]
          ;; Logged In
          (list
            [:a {:href "/users" :title "Account Info"}
              (:name user)]
            [:span " "]
            [:a {:href "/logout" :title "Log Out"}
              [:i.fas.fa-sign-out-alt]]))]
      [:div.clear-fix]]))

(defn build-main-nav
  "Build HTML fragment for info/settings navigation."
  [user]
  (html
    [:b "TODO: main nav"]))

(defn template-page
  "Common template used by all pages on the site.

  * `req`
    * The ring request map
  * `title`
    * The title of the page
  * `content`
    * Hiccup structure containing the body of the page
  * `user`
    * The currently logged in user (if any)
  * `css-files`
    * An optional list of CSS files to include
  * `script-files`
    * An optional list of Javascript files to include"
  [req title content & {:keys [user css-files script-files]}]
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

       ;; Scripts (dependencies)
       [:script
         {:defer ""
          :src "/deps/font-awesome/svg-with-js/js/fontawesome-all.min.js"}]
       [:script {:src "/deps/lodash/lodash.min.js"}]
       [:script {:src "/deps/momentjs/moment.min.js"}]
       [:script {:src "/deps/jquery/jquery-2.1.3.min.js"}]
       [:script {:src "/deps/rxjs/rx.all.min.js"}]
       [:script {:src "/deps/markedjs/marked.min.js"}]

       ;; Scripts (domain)
       [:script {:src (str "/js/utils.js?v=" (:version @config))}]
       [:script {:src (str "/js/main.js?v=" (:version @config))}]
       (if (non-empty? script-files)
         (for [sf script-files]
           [:script {:src (str "/js/" sf "?v=" (:version @config))}]))]

      [:body
        ;; Used for modal dialogs
        [:div#overlay.hidden]

        (build-top-header user)

        (build-main-nav user)

        ;; Page content
        [:main#app
          [:div {:id (gen-main-id title)} content]]

        ;; ClojureScript
        [:script {:src "/cljs/main.js"}]])))
