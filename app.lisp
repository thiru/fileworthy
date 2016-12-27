#||
# Fileworthy

## Introduction

* Fileworthy is intended to facilitate easy management of files and notes across many devices

### Goals and Motivation

* My primary, and preferred workflow is editing my files off my **local** drive in [Neovim](https://neovim.io/)
  * and I don't want this to change
* But I'd also like to be able to view all my files and directories as a website
  * especially when I'm on the go
  * and with a small mobile device
* The website should basically be a simple reprentation of the contents of one or more directories
* The notes would ideally be written in a markup language such as [Markdown](https://guides.github.com/features/mastering-markdown/) so that:
  * they can be easily transformed to other structured formats like HTML
  * easily edited with any text editor
  * easily manipulated by other tools
    * as we're just dealing with **plain text**
* The website should also support
  * editing of text files
  * managing binary files
* To keep the files on your local file-system and the website in sync
  * consider using a 3rd-party app like [Syncthing](https://syncthing.net)
* Configuration should be optional, with sensible defaults chosen
  * i.e. you shouldn't have to structure your files in any particular way
  * or include any special template files, etc.
  * and least of all, require any complex setup
  * though these options should be available if needed

### Proposed Solution

* Create an app that will display the contents of one or more directories as a regular website
* Any file can be downloaded
* If the browser natively supports the file format
  * e.g. plain text, images, movies, etc.
  * it will render it as is
  * and an option to specify additional file formats as plain text so they can be viewed as such
* Markdown files will be transformed to HTML
* Directories will simply be links to other pages
* All files and directories can be further managed in the following ways:
  * create new files/folders
  * upload files/folders
  * rename existing files/folders
  * edit text files through a web interface
    * including source (Markdown) files from which an HTML page was generated
  * delete files/folders
* Configuration files are optional
  * i.e. use sensible defaults
  * of course this will be biased on *my* initial desires

### Point-Form Grammar

* You might've noticed that just about everything is written in point-form
* I love point-form
* I kind of view this as a sort of grammar
  * where nested ideas are expressed as nested points
  * without the need to use much punctuation
* I'm not sure if this is easier for casual, beginning-to-end reading
  * but I find it a lot easier for scanning and browsing
  * and also to grasp complex ideas with lots of nesting
* I've been taking all my notes in this form for over a decade
  * and it's served me very well
* But you may very well find this form annoying
  * so appologies in advance :)

## Coding Choices

### Language & Environment

* The app will be written in Common Lisp
* I'm primarily using [SBCL](http://www.sbcl.org/)
  * but the code should work in most modern Common Lisp implementations
  * I'm not explicitly choosing to depend on any features specific to SBCL
    * but I'm not sure the same is true for all libraries used

### Literate Programming

* This will also serve as my first foray into [literate programming](http://www.literateprogramming.com/knuthweb.pdf)
* However I'd rather not go the traditional web/weave/tangle approach
* Instead I want to take a quasi-literate approach where
  * all the explantory text/documention and source code is in a single Common Lisp file
    * i.e. [app.lisp](../app.lisp)
  * the explantory text will be exclusively written
    * within Common Lisp's [multi-line comments](http://clhs.lisp.se/Body/02_dhs.htm)
    * in (Github flavoured) Markdown syntax
* Obviously there are drawbacks to this approach
  * with the biggest one potentially being that I can't really write this in
    a *stream of conciousness* flow
  * instead I'll be taking a (mostly) top-down approach
* But there are also considerable advantages:
  * The code is easily run, edited and debugged within your favourite editor
  * There's no need for the tangle phase
  * The documentation can be easily generated
    * see [weave.ros](../weave.ros)

### Code Conventions & Style

* Many developers believe **managing complexity** to be the biggest challenge in software development
  * so the primary motivation behind the conventions and styles used here are to mitigate this
  * i.e. the code must be as simple as possible for **humans** to **read** and **maintain**
* The code will generally follow a combination of functional and procedural programming, avoiding OOP
  * e.g. inheritance, polymorphism, etc.
  * more and more, developers are finding these constructs
    * at least, as they are commonly implemented
    * to make code even harder to understand
  * I particuarly like the pitfalls pointed out by the folks behind [DCI](https://en.wikipedia.org/wiki/Data,_context_and_interaction)
    * although I don't agree with many of their other ideals
* Structs are used throughout the codebase over classes since
  * they have a good default textual representation
  * are far less complex
  * still a good way to group a set of related data

## Source Files

* What follows is a brief description of the main files and folders the compose this app:
* [app.lisp](../app.lisp)
  * Contains **all** the code and documentation
* [docs/](../docs)
  * [index.html](index.html)
    * The sole documentation file
    * Generated from
      * [index.md](index.md)
      * [template.html](template.html)
  * [index.md](index.md)
    * Generated from [app.lisp](../app.lisp)
  * [highlightjs](highlightjs/)
    * Directory containing the syntax highlighting library used by the documentation:
      * [highlight.js](https://highlightjs.org/)
  * [template.html](template.html)
    * HTML template for this file
    * Basically contains everything outside the body tag
* [EULA](../EULA)
  * End-user license agreement
* [fileworthy.asd](../fileworthy.asd)
  * ASDF system/build description
* [index.html](../index.html)
  * Simply redirects to the documentation
  * This file is needed here to facilitate hosting from github.io
* [LICENSE](../LICENSE)
  * GPLv3 license
* [README.md](../README.md)
  * Basically contains the intro of this file
* [start.lisp](../start.lisp)
  * Common Lisp script to start the website with default options
* [start.sh](../start.sh)
  * Shell script to start the website within a GNU screen session
* [static/](../static/)
  * Contains static web resources such as images, Javascript, CSS, etc.
* [version](../version)
  * Version of the app
  * The version will follow a simple MAJOR.MINOR form
    * A change to the major version indicates
      * significant changes
      * or breaking changes
    * A change to the minor version indicates
      * non-major changes
      * changes can be bug fixes and features
    * 0.x version changes are considered alpha/beta
      * and so will regularly introduce breaking changes
  * The version is quoted so we can easily read the file through a Common Lisp function as a string
* [weave.ros](../weave.ros)
  * Common Lisp script that transforms a Common Lisp source file into Markdown and HTML
  * it can also monitor the source file for changes via [inotify tools](https://github.com/rvoicilas/inotify-tools)
  * [Roswell](https://github.com/roswell/roswell) is needed to run this script, e.g.:
    * `./weave.ros -m app.lisp`
    * This will monitor *app.lisp* and generate the following whenever it changes:
      * *./docs/index.md*
      * *./docs/index.html*

## Libraries

* The following libaries are used by this app:
* [Alexandria](https://common-lisp.net/project/alexandria/)
  * Minimal utility library that seems to be highly recommended
* [Clack](http://clacklisp.org/)
  * Abstract web framework library
  * Sits on top of tools like Hunchentoot, Woo, etc.
* [CL-MARKUP](https://github.com/arielnetworks/cl-markup)
  * Easy way to generate HTML in Lisp-friendly syntax
* [CL-PPCRE](http://weitz.de/cl-ppcre/)
  * Defacto regular expression library
* [LOCAL-TIME](https://common-lisp.net/project/local-time/)
  * Easily manipulate and display date and times
* [Ningle](https://github.com/fukamachi/ningle)
  * Minimal web framework
  * There's also [Caveman2](https://github.com/fukamachi/caveman) but I don't think I'll need it's capabilities
* [SPLIT-SEQUENCE](http://www.cliki.net/split-sequence)
  * Easily split sequences by arbitrary delimiters
* [UIOP](https://github.com/fare/asdf/tree/master/uiop)
  * Portable, OS and file-system utilities
  * This is actually part of the core of [ASDF](https://github.com/fare/asdf)

## System Definition

* Okay, on to the code finally!
* See [fileworthy.asd](../fileworthy.asd) for the ASDF build system definition
  * describing high-level project details
  * and dependencies needed to build the project
* This needs to be a separate file to work with some tools
  * e.g. ASDF and [Quicklisp](https://www.quicklisp.org/)
  * otherwise I might've embedded it here
* All of the other code will be in a single file:
  * [app.lisp](../app.lisp)
  * I'm not sure if this is a good idea
  * I'm choosing to go this route as it simplifies my [current tooling](../weave.ros) with Literate Programming

## Package Definition

* The community recommends defining the package within `CL-USER`:
||#
(in-package :cl-user)

#||
* We'll use a single package for the entire application
* I'm not sure if this is a good idea
  * just trying out a different approach
  * let's see how it works out
||#
(defpackage :fileworthy
  (:use :cl :cl-markup :ningle :glu :local-time :split-sequence :uiop)
  (:documentation "A simple website to manage your *local* notes and files across all your devices")
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:export :*app* :start :stop :restart-app))

#||
* Switch back to the sole package of the app
||#
(in-package :fileworthy)

#||
## Global Variables

* I'm trying to keep the number of global objects as small as possible
* `*APP*` will contain most of the common properties grouped in a single struct
* It will be initialised/reinitialised when `START` or `RESTART-APP` is called
  * so we don't bother initialising it here
||#
(defvar *app*
  nil
  "Singleton instance containing general app details.")

#||
* `*HANDLER*` gets initialised/reinitialised when `START` or `RESTART-APP` is called
  * so we don't bother initialising it here
||#
(defvar *handler*
  nil
  "Singleton Ningle web handler.")

#||
* Unlike the other global variables `*WEB*` need only be initialised once
  * and it doesn't require any other objects to be initialised first
  * so we do so right here
||#
(defvar *web*
  (make-instance 'ningle:<app>)
  "Singleton Ningle web application instance.")

#||

## APP

* The `APP` struct groups general, high-level app details including
  * the name of this instance of the app
    * this will default to the name of base-dir
  * the base/root directory of the app
  * the version of the app
  * the time the app was last updated
    * based on the last write time of the [version file](../version)
  * the directory containing static web resources
  * the regular expression defining static resources
||#
(defstruct app
  "Contains general, high-level app details."
  (name "" :type STRING)
  (base-dir nil :type PATHNAME)
  (version "0.0" :type STRING)
  (last-updated nil :type TIMESTAMP)
  (web-static-dir nil :type PATHNAME)
  (web-static-regex "^(?:/images/|/css/|/deps/|/js/|/robot\\.txt$|$)"
                    :read-only t))

#||

### CREATE-APP
* This function creates an instance of `APP`
  * with all fields correctly initialised
* Note that `APP-VERSION` is loaded from a separate file, [version](../version)
  * this is partly due to [fileworthy.asd](../fileworthy.asd) needing access to the app version as well
  * this way we have a single place where the version gets updated
    * and it's easily modified and read
||#
(defun create-app ()
  "Create APP instance."
  (let ((base-dir (asdf:system-source-directory :fileworthy))
        (version-file-path (asdf:system-relative-pathname
                            :fileworthy
                            "version")))
    (make-app :name
              (last1 
                (split-sequence
                  #\/
                  (princ-to-string (uiop/filesystem:truename* base-dir))
                  :remove-empty-subseqs t))
              :base-dir base-dir 
              :version (asdf::read-file-form version-file-path)
              :last-updated
              (universal-to-timestamp
                (file-write-date version-file-path))
              :web-static-dir (merge-pathnames #P"static/" base-dir))))

#||
## Startup and Shutdown

* To launch the website with the default values we need only call `START`

### `START`

* This function starts the app
  * including the underlying web server
  * by default at http://localhost:9090
* `START` can be called more than once
  * even without calling `STOP`
* If the web server is already running a debugger restart will be presented
  * with the option to restart to web server
* Also note that `*APP*` gets reinitialised even if it's already loaded
  * not sure this is necessary but it may be useful to reload the version file
  * and it's not an expensive operation anyway
* Parameters:
  * `SERVER`
    * defines the backend server to use
    * see [this page](http://clacklisp.org/) for a list of supported servers
    * the creator of [Ningle](https://github.com/fukamachi/ningle) recommends using
      * [Hunchentoot](http://weitz.de/hunchentoot/) in **development**
      * [Woo](https://github.com/fukamachi/woo) in **production**
      * which is why the default value is Hunchentoot
  * `PORT`
    * the port of the web server
    * The default is 9090 as this is Hunchentoot's default
  * `DEBUG`
    * whether to start the web server in debug mode
||#
(defun start (&key (server :hunchentoot) (port 9090) (debug t))
  "Starts the app."
  (setf *app* (create-app))

  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))

  (setf *handler* (create-web-handler server port debug))
  (define-routes)
  (format t "Started Fileworthy ~A~%" (app-version *app*)))

#||
### `STOP`

* This function gracefully shuts down the app
  * including the underlying web server
||#
(defun stop ()
  "Stops the app."
  (if *handler*
   (prog1
    (clack:stop *handler*)
    (setf *handler* nil)
    (format t "Stopped Fileworthy ~A~%" (app-version *app*)))))

#||
### `RESTART-APP`

* This function restarts the app
* I would've named this function `RESTART`
  * but it would then conflict with the [system class](http://clhs.lisp.se/Body/t_rst.htm#restart) of the same name
||#
(defun restart-app ()
  "Restart the app."
  (stop)
  (start))


#||
### `CREATE-WEB-HANDLER`

* This function creates the Ningle web handler
* `*WEB*` is expected to be properly initialised before this function is called
* The BUILDER macro defines web middleware
* For now I'm only using it to define where to get the static resources
  * like CSS, Javascript, images, etc.
* See `START` for a description of the parameters
  * as it uses the exact same list
||#
(defun create-web-handler (server port debug)
  "Create the singleton Ningle web handler."
  (clack:clackup
    (builder
      (:static
        :path
        (lambda (path)
          (if (scan (app-web-static-regex *app*) path)
            path
            nil))
        :root (app-web-static-dir *app*))
      *web*)
    :server server
    :port port
    :debug debug))

#||
## Core Domain Logic

### `GET-DIR-NAMES`

* This function gets a list of directory names relative to either
  * the given directory `PARENT`
  * or the root folder as specified by `APP-BASE-DIR`
||#
(defun get-dir-names (&optional parent)
  "Get directory names."
  (map 'list
       (λ (abs-dir)
          (last1 (split-sequence #\/
                                 (princ-to-string abs-dir)
                                 :remove-empty-subseqs t)))
       (uiop/filesystem:subdirectories (or parent (app-base-dir *app*)))))
#||
### `GET-FILE-NAMES`

* This function gets a list of file names relative to either
  * the given directory `PARENT`
  * or the root folder as specified by `APP-BASE-DIR`
||#
(defun get-file-names (&optional parent)
  "Get file names."
  (map 'list
       (λ (abs-file)
          (last1 (split-sequence #\/
                                 (princ-to-string abs-file)
                                 :remove-empty-subseqs t)))
       (uiop/filesystem:directory-files
         (or parent (app-base-dir *app*)))))

#||
### `GET-FILE-CONTENT`
||#
(defun get-file-content (path)
  "Get file contents of `PATH`."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


#||
## Web Utils

* This section contains utility functions common to most web functionality

### `EXTRACT-URL-PATHNAME`

* When a route is defined via `(SETF (ROUTE ...))`
  * the route handling function is passed a `PARAMS` object
  * that contains details on the requested URL
||#
(defun extract-url-pathname (ningle-params)
  "Get the URL path name of the given Ningle params object."
  (first (cdr (assoc :splat ningle-params))))

#||
## Web Resource Routes

* We define the routes in a function
  * as we need an instance of `*WEB*` properly initialised first
  * and this is done only after `START` is called
||#

(defun define-routes ()
  "Define web resource routes."

  ;; File-system path page
  (setf (route *web* "/*" :method :GET) #'page-fs-path))

#||
## Web Pages

* All web page-related functions will be prefixed with `PAGE-`

### `PAGE-TEMPLATE`

* This function defines the template all pages will use
* Parameters:
  * `PARAMS`
    * the Ningle URL PARAMS object
  * `TITLE`
    * the title of the page
    * note that the given title is
      * suffixed with the site's name and project name (Fileworthy)
      * and separated by a hyphen
      * e.g. "Home - My Documents - Fileworthy"
  * `CONTENT`
    * the HTML of the page as a raw string
    * note that the caller is responsible for properly escaping special characters
||#
(defun page-template (params title content)
  "Base template for all web pages."
  (let* ((path-name (extract-url-pathname params))
         (path-segs (split-sequence #\/ path-name :remove-empty-subseqs t))
         (first-path-seg (first path-segs)))
    (html5 :lang "en"
           (:head
             (:meta :charset "utf-8")
             (:meta :http-equiv "X-UA-Compatible" :content "IE=edge")
             (:meta
               :name "viewport"
               :content "width=device-width, initial-scale=1")
             (:title (sf "~A - ~A - Fileworthy" title (app-name *app*)))
             (:link :rel "shortcut icon" :href "/images/favicon.ico")
             (:link
               :href "/deps/font-awesome/css/font-awesome.min.css"
               :rel "stylesheet"
               :type "text/css")
             (:link :href "/css/main.css" :rel "stylesheet")
             (:script :src "/deps/lodash/lodash.min.js" "")
             (:script :src "/deps/momentjs/moment.min.js" "")
             (:script :src "/deps/markedjs/marked.min.js" ""))
           (:body
             ;; Top Bar
             (:header :id "top-bar"
              (:a :id "app-name" :href "/"
               (app-name *app*))
              (:a :id "project-name" :href "/"
               "Fileworthy "
               (:span
                 :id "version"
                 :title (sf "Updated ~A" (app-last-updated *app*))
                 (sf "~A" (app-version *app*))))
              (:div :class "clear-fix"))
             (:nav
               ;; Root Folders
               (:ul :id "root-folder-names"
                (:li
                  (:a :id "root-folder" :href "/" :title "Root"
                   (:i :class "fa fa-folder-open" "")))
                (loop
                  :for dir-name :in (get-dir-names)
                  :collect (markup
                            (:li
                              (:a
                                :class
                                (if (string= first-path-seg dir-name)
                                  "selected"
                                  nil)
                                :href (sf "/~A/" dir-name)
                                (string-capitalize dir-name))))))
               ;; Sub-folders
               (let* ((expanded-dirs (expand-all-folders path-name))
                      (sub-dir-name-lst (map 'list
                                        (λ (sub-dir) (get-dir-names sub-dir))
                                        expanded-dirs)))
                 (loop :for sub-dir-names :in sub-dir-name-lst
                       :for i :from 0
                       :collect
                       (markup
                         (:ul :class "sub-folder-names"
                          (loop :for dir-name :in sub-dir-names
                                :collect
                                (markup
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
                                      dir-name)))))))))
             (:main
               (raw content))
             (:script :src "/js/main.js" "")))))

#||

* An example call of the following is:
  * "root/sub1/sub1a" ==> '("root", "root/sub1", "root/sub1/sub1a")
||#
(defun expand-all-folders (path-name)
  "Expand all the path segments in PATH-NAME to a list of sub-folders."
  (let* ((path-name (string-trim '(#\/) (or path-name ""))))
    (if (empty? path-name)
      (return-from expand-all-folders nil))
    (loop :for c :across path-name
          :for i :from 0
          :when (char= #\/ c)
          :collect (subseq path-name 0 i) :into lst
          :finally (return (append lst (list path-name))))))
#||
### `PAGE-ERROR-NOT-FOUND`

* This is the standard 404 (not found) page.
||#
(defun page-error-not-found (params)
  "Not Found error page."
  (setf (lack.response:response-status *response*) 404)
  (page-template
    params
    "Not Found"
    (markup
      (:h2 "Not Found")
      (:p "The page or resource you requested could not be found.")
      (:p
        (:a :href "/"
         (:i :class "fa fa-home" "")
         (:b " Go back to the home page"))))))

#||
### `PAGE-FS-PATH`
||#
(defun page-fs-path (params)
  "File-system path page."
  (let* ((path-name (extract-url-pathname params))
         (path-segs (split-sequence #\/ path-name :remove-empty-subseqs t))
         (abs-fs-path (get-fs-path-from-url path-name))
         (rel-fs-path (if abs-fs-path (subpathp abs-fs-path (app-base-dir *app*))))
         (file-content "")
         (file-names (get-file-names abs-fs-path)))
    (cond
      ;; Show File
      ((file-exists-p abs-fs-path)
       (setf file-content (get-file-content abs-fs-path))    
       (page-template
         params
         rel-fs-path
         (markup
           (:p
             (:i :class "fa fa-file" "")
             (:span " ")
             (:span :id "file-path" rel-fs-path))
           (:ul :id "files" :class "file-names col"
            (loop
              :for file-path :in file-names
              :collect (markup
                        (:li
                          (:a
                            :class
                            (if (string= file-path (last1 path-segs))
                              "selected"
                              nil)
                            :href file-path
                            file-path)))))
           (:pre :id "raw-file-content" :class "col hidden" file-content)
           (:div :id "gen-file-content" :class "col"))))
      ;; Show Directory
      ((directory-exists-p abs-fs-path)
       (if (= 1 (length file-names))
           (setf file-content (get-file-content (sf "~A~A" abs-fs-path (first file-names)))))
       (page-template
         params
         (if (empty? rel-fs-path) "Home" rel-fs-path)
         (markup
           (:p
             (:i :class "fa fa-folder-open" "")
             (:span " ")
             (:span (if (empty? rel-fs-path)
                      "/"
                      (to-string rel-fs-path))))
           (:ul :id "files" :class "file-names col"
            (loop
              :for file-path :in file-names
              :collect (markup
                        (:li
                          (:a
                            :class
                            (if (and (= 1 (length file-names))
                                     (string= file-path (first file-names)))
                              "selected"
                              nil)
                            :href file-path
                            file-path)))))
           (:pre :id "raw-file-content" :class "col hidden" file-content)
           (:div :id "gen-file-content" :class "col"))))
      ;; Path Not Found
      (t (page-error-not-found params)))))

(defun get-fs-path-from-url (path-name)
  "Gets an absolute local file-system path from the given path name."
  (let* ((path (merge-pathnames* path-name)))
    (if (subpathp path (app-base-dir *app*))
      path)))
