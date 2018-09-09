(ns fileworthy.web.pages.layout
  (:require
            [clojure.string :as string]

            [reagent.session :as session]

            [fileworthy.core.users :as users]
            [fileworthy.web.pages.about :as about-page]
            [fileworthy.web.pages.home :as home-page]
            [fileworthy.web.pages.login :as login-page]
            [fileworthy.web.pages.logout :as logout-page]
            [fileworthy.web.pages.not-found :as not-found-page]
            [fileworthy.web.state :refer [state]]
            [fileworthy.web.utils :as utils]))

(defn modal-dialog-ui
  "HTML fragment for overlay element used for modal dialogs."
  []
  [:div#overlay.hidden])

(defn top-header-ui
  "HTML fragment for the site-wide top header."
  []
  [:div
    [:header#top-bar
      ;; Info/Settings Icon (hamburger menu)
      [:a#ham-menu
        {:href "javascript:void(0)"
         :on-click (fn [e]
                     (.preventDefault e)
                     (swap! state
                            assoc-in [:settings-nav :visible?]
                            (not (-> @state :settings-nav :visible?))))
         :title "Info/Settings"}
        [:i.fas.fa-bars]]
      [:span " "]
      ;; Site Name
      [:a#app-name {:href "/" :title "Home"}
        (-> @state :site-info :site-name)]
      ;; User Info
      [:div#user-info
        (if (empty? (:user @state))
          ;; Logged Out
          [:a {:href "/login"}
            [:i.fas.fa-sign-in-alt]
            " Log In"]
          ;; Logged In
          [:span
            [:a {:href "/users" :title "Account Info"}
              (-> @state :user :name)]
            [:span " "]
            [:a {:on-click logout-page/do-logout
                 :href "javascript:void(0)"
                 :title "Log Out"}
              [:i.fas.fa-sign-out-alt]]])]
      [:div.clear-fix]]])

(defn settings-nav-ui
  "HTML fragment for info/settings navigation."
  []
  (let [nav-visible? (-> @state :settings-nav :visible?)]
    [:nav
      [:ul#info-menu.flat-list
        {:class (if (not nav-visible?) "hidden")}
        (if (users/is-admin? (:user @state))
          [:li
            [:a {:href "/settings"}
              [:i.fas.fa-cog]
              " Settings"]])
        (if (not (empty? (:user @state)))
          [:li
            [:a {:href "/users/me"}
              [:i.fas.fa-user]
              " Account"]])
        (if (users/is-admin? (:user @state))
          [:li
            [:a {:href "/users"}
              [:i.fas.fa-users]
              " Users"]])
        [:li
          [:a {:href "/about"}
            [:i.fas.fa-info-circle]
            " About"]]]]))

(defn page-ui
  "Common layout of all pages on the site."
  []
  (let [page-id (-> @state :current-route :page-id)
        page-var (-> @state :current-route :page-var)
        title (:title (meta page-var))]
    (utils/set-page-title title)
    [:div
      (modal-dialog-ui)
      (top-header-ui)
      (settings-nav-ui)
      ;; An id is attached to the main element primarly to target page-specific
      ;; styles to it.
      [:main {:id (name page-id)}
       ^{:key page-id} [page-var]]]))
