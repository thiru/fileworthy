(in-package :fileworthy)

#||

### `PAGE-TEMPLATE`

* This function defines the template all pages will use
* Parameters:
  * `TITLE`
    * the title of the page
    * note that the given title is
      * suffixed with the site's name and project name (Fileworthy)
      * and separated by a hyphen
      * e.g. "Home - My Documents - Fileworthy"
  * `PAGE-ID`
    * the unique id of the page set on main element
  * `CONTENT`
    * the HTML of the page as a raw string
    * note that the caller is responsible for properly escaping special characters
      * TODO: review this claim
||#
(defun page-template (title page-id content)
  "Base template for all web pages."
  (let* ((rrp (config-reserved-resource-path *config*))
         (user (empty 'user :unless (session-value 'user)))
         (path-name (script-name* *request*))
         (path-segs (split-sequence #\/ path-name :remove-empty-subseqs t))
         (first-path-seg (first path-segs))
         (fw-info-page? (string-equal rrp first-path-seg)))
    (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
      (:html :lang "en"
           (:head
             (:meta :charset "utf-8")
             (:meta :name "robots" :content "noindex, nofollow")
             (:meta :http-equiv "X-UA-Compatible" :content "IE=edge")
             (:meta
               :name "viewport"
               :content "width=device-width, initial-scale=1")
             (:title
               (if (blank? (config-site-name *config*))
                 (str title)
                 (fmt "~A - ~A"
                      title
                      (config-site-name *config*))))
             (:link :href "/images/favicon.ico" :rel "shortcut icon")
             (:link
               :href
               (sf "/~A/deps/font-awesome/css/font-awesome.min.css" rrp)
               :rel "stylesheet")
             (:link
               :href (sf "/~A/deps/highlightjs/styles/monokai.css" rrp)
               :rel "stylesheet")
             (:link
               :href (sf "/~A/css/main.css?v=~A" rrp (app-version *app*))
               :rel "stylesheet")
             (:link
               :href (sf "/~A/css/main.mobile.css?v=~A"
                         rrp
                         (app-version *app*))
               :rel "stylesheet")
             (:link
               :href (sf "/~A/css/main.print.css?v=~A" rrp (app-version *app*))
               :rel "stylesheet")

             (:script :src (sf "/~A/deps/lodash/lodash.min.js" rrp) "")
             (:script :src (sf "/~A/deps/rxjs/Rx.min.js" rrp) "")
             (:script :src (sf "/~A/deps/momentjs/moment.min.js" rrp) "")
             (:script :src (sf "/~A/deps/markedjs/marked.min.js" rrp) "")
             (:script
               :src (sf "/~A/deps/highlightjs/highlight.pack.js" rrp) "")
             (:script
               :src (sf "/~A/js/utils.js?v=~A" rrp (app-version *app*)) "")
             (:script
               :src (sf "/~A/js/main.js?v=~A" rrp (app-version *app*)) ""))
           (:body
             :data-rrp rrp
             ;; Overlay (for dialogs)
             (:div :id "overlay" :class "hidden" " ")
             ;; Top Bar
             (:header :id "top-bar"
              ;; Menu Icon
              (:a
                :href "javascript:site.toggleMenu()"
                :title "Settings"
                (:i :class "fa fa-bars" " "))
              ;; Site Name
              (:a :id "app-name" :href "/" :title "Home"
               (str (config-site-name *config*)))
              ;; User Info
              (:div :id "user-info"
               (if (empty? user)
                 ;; Logged Out
                 (htm
                     (:a :href "javascript:site.showLogin()"
                      (:i :class "fa fa-sign-in" "")
                      " Log In"))
                 ;; Logged In
                 (htm
                     (:a
                       :href (url-for user)
                       (str (user-name user)))
                     (:span " ")
                     (:a
                       :href (sf "/~A/logout" rrp)
                       :title "Log Out"
                       (:i :class "fa fa-sign-out" "")))))
              (:div :class "clear-fix"))
             ;; Fileworthy Info/Settings
             (:ul
               :id "info-menu"
               :class (if fw-info-page? "flat-list" "flat-list hidden")
               (:li
                 :class (if (string-equal "about" (nth 1 path-segs))
                          "selected")
                 (:a :href (url-for 'about)
                  (:i :class "fa fa-info-circle" "")
                  " About"))
               (if (user-admin? user)
                 (htm
                   (:li
                     :class (if (string-equal "settings"
                                              (nth 1 path-segs))
                              "selected")
                     (:a :href (url-for 'settings)
                      (:i :class "fa fa-cog" "")
                      " Settings"))))
               (if (not (empty? user))
                 (htm
                   (:li
                     :class (if (and (string-equal
                                       "users"
                                       (nth 1 path-segs))
                                     (string-equal
                                       (to-string (user-id user))
                                       (nth 2 path-segs)))
                              "selected")
                     (:a
                       :href (url-for user)
                       (:i :class "fa fa-user" "")
                       " My Account"))))
               (if (user-admin? user)
                 (htm
                   (:li
                     :class (if (and (string-equal "users"
                                                   (nth 1 path-segs))
                                     (empty? (nth 2 path-segs)))
                              "selected")
                     (:a :href (url-for 'users)
                      (:i :class "fa fa-users" "")
                      " Users")))))
             (if (or (config-allow-anonymous-read *config*)
                     (not (empty? user)))
               (htm
                   (:nav
                     (:ul :id "main-menu-items" :class "flat-list"
                      ;; Root Folders
                      (loop :for dir-name :in (get-dir-names user)
                            :collect (htm
                                       (:li
                                         (:a
                                           :class
                                           (if (string= first-path-seg dir-name)
                                             "selected"
                                             nil)
                                           :href (sf "/~A/" dir-name)
                                           (str dir-name))))))
                     ;; Sub-folders
                     (let* ((expanded-dirs (expand-sub-dirs path-name))
                            (sub-dir-name-lst (map 'list
                                                   (λ (sub-dir)
                                                      (get-dir-names
                                                        user
                                                        sub-dir))
                                                   expanded-dirs)))
                       (loop :for sub-dir-names :in sub-dir-name-lst
                             :for i :from 0
                             :when (not (empty? sub-dir-names))
                             :collect
                             (htm
                               (:ul :class "sub-menu-items flat-list"
                                (loop :for dir-name :in sub-dir-names
                                      :collect
                                      (htm
                                        (:li
                                          (:a
                                            :class
                                            (if (string= dir-name
                                                         (nth (1+ i) path-segs))
                                              "selected"
                                              nil)
                                            :href (sf "/~A/~A/"
                                                      (nth i expanded-dirs)
                                                      dir-name)
                                            (str dir-name))))))))))))
             (:main :id page-id
              (str content))
             ;; Login Dialog
             (:section :id "login-dialog" :class "dialog"
              (:div :class "dialog-content"
               (:h2 "Welcome!")
               (:p
                 (:input
                   :id "username"
                   :class "full-width"
                   :onkeyup "ui.onEnter(event, site.login)"
                   :placeholder "Username/Email Address"
                   :title "Username/Email Address"
                   :type "text"))
               (:p
                 (:input
                   :id "password"
                   :class "full-width"
                   :onkeyup "ui.onEnter(event, site.login)"
                   :placeholder "Password"
                   :title "Password"
                   :type "password"))
               (:p :id "login-result")
               (:p
                 (:a
                   :id "login-btn"
                   :class "button full-width"
                   :href "javascript:site.login()"
                   "Log In"))
               (:p
                 (:a
                   :id "forgot-pwd"
                   :href "javascript:forgotPwd()"
                   :style "float:left"
                   "Forgot password")
                 (:a
                   :href "javascript:site.closeLogin()"
                   :style "float:right"
                   "Close")))))))))

#||

* An example call of the following is:
  * "root/sub1/sub1a" ==> '("root", "root/sub1", "root/sub1/sub1a")
||#
(defun expand-sub-dirs (path-name)
  "Expand all the path segments in `PATH-NAME` to a list of sub-directories."
  (let* ((path-name (string-trim '(#\/) (or path-name ""))))
    (if (empty? path-name)
      (return-from expand-sub-dirs '()))
    (loop :for c :across path-name
          :for i :from 0
          :when (char= #\/ c)
          :collect (subseq path-name 0 i) :into lst
          :finally (return (append lst (list path-name))))))
