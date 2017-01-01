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

#### Programming Paradigm

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

#### Error Handling

* Errors/exceptions are triggered only in truly unexpected cases
  * i.e. due to a programmer error
  * or an unrecoverable error leaving the app in a bad state
  * though the same cannot be said for dependencies
* So, many functions will return a value indicating a result/error code
  * this is encapsulated in an `R` struct
  * see [glu.lisp](../glu.lisp)

## Source Files

* What follows is a brief description of the main files and folders the compose this app:
* [app.lisp](../app.lisp)
  * Contains just about **all** of the code and documentation relevant to this project
* [docs/](../docs)
  * [index.html](index.html)
    * The sole documentation file
    * Generated from
      * [index.md](index.md)
      * [template.html](template.html)
  * [index.md](index.md)
    * Generated from [app.lisp](../app.lisp)
  * [highlightjs](highlightjs/)
    * Directory containing the syntax highlighting library used by the documentation, [highlight.js](https://highlightjs.org/)
  * [template.html](template.html)
    * HTML template for this file
    * Basically contains everything outside the body tag
* [EULA](../EULA)
  * End-user license agreement
* [fileworthy.asd](../fileworthy.asd)
  * ASDF system/build description
* [glu.lisp](../glu.lisp)
  * GLU stands for Global Lisp Utilities
  * These are my (opinionated) utilities that are not particular to this app per se
* [index.html](../index.html)
  * Simply redirects to the documentation
  * This file is needed here to facilitate hosting from github.io
* [LICENSE](../LICENSE)
  * GPLv3 license
* [package.lisp](../package.lisp)
  * Contains the sole package definition for this app
* [README.md](../README.md)
  * Basically contains the intro of this file
* [start.lisp](../start.lisp)
  * Common Lisp script to start the website with default options
* [start.sh](../start.sh)
  * Shell script to start the website within a GNU screen session
* [static/](../static/)
  * Contains static web resources such as CSS, images, Javascript and dependencies
* [version](../version)
  * Current version of the app
  * The version will follow a simple MAJOR.MINOR form
    * A change to the major version indicates
      * significant changes
      * or breaking changes
    * A change to the minor version indicates
      * non-major changes
      * changes can be bug fixes and features
    * *0.x* versions are considered alpha/beta
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

### Back-end/Server-side

* [Alexandria](https://common-lisp.net/project/alexandria/)
  * Minimal utility library that seems to be highly recommended
* [Clack](http://clacklisp.org/)
  * Abstract web framework library
  * Sits on top of tools like Hunchentoot, Woo, etc.
* [CL-MARKUP](https://github.com/arielnetworks/cl-markup)
  * Easy way to generate HTML in Lisp-friendly syntax
* [CL-PPCRE](http://weitz.de/cl-ppcre/)
  * Defacto regular expression library
* [Hunchentoot](http://weitz.de/hunchentoot/)
  * Web server and framework
* [LOCAL-TIME](https://common-lisp.net/project/local-time/)
  * Easily manipulate and display date and times
* [SPLIT-SEQUENCE](http://www.cliki.net/split-sequence)
  * Easily split sequences by arbitrary delimiters
* [UIOP](https://github.com/fare/asdf/tree/master/uiop)
  * Portable, OS and file-system utilities
  * This is actually part of the core of [ASDF](https://github.com/fare/asdf)

### Front-end/Client-side

* [Font Awesome](http://fontawesome.io/)
  * Rich set of icons as a font
* [Highlight.js](https://highlightjs.org/)
  * Code/syntax highlighting
* [Lodash](https://lodash.com/)
  * Utility library with many useful functional list operations
* [Marked.js](https://github.com/chjj/marked)
  * Transforms Markdown to HTML
* [Moment.js](http://momentjs.com/)
  * Date/time utilities

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

* We'll use a single package named `FILEWORTHY` for the entire application
* I'm not sure if this is a good idea
  * just trying out a different approach
  * let's see how it works out
* See [package.lisp](../package.lisp) for the code
* The reason it's in a separate file is so that we can have the package
  defined before we compile the source files that depend on it
  * e.g. [glu.lisp](../glu.lisp)

```lisp
(in-package :fileworthy)


```

## Global Variables

* I'm trying to keep the number of global objects as small as possible
* `*APP*` will contain most of the common properties grouped in a single struct
* It will be initialised/reinitialised when `START-APP` or `RESTART-APP` is called
  * so we don't bother initialising it here

```lisp
(defvar *app*
  nil
  "Singleton instance containing general app details.")


```

* `*ACCEPTOR*` gets initialised/reinitialised when `START-APP` or `RESTART-APP` is called
  * so we don't bother initialising it here

```lisp
(defvar *acceptor*
  nil
  "Singleton Hunchentoot web handler.")


```

## APP

* The `APP` struct groups general, high-level app details including
  * `NAME`
    * the user-specified name of this instance of the app
    * this will default to the name of the directory specified in `working-dir`
  * `APP-DIR`
    * the root directory of the app's source/binaries
  * `WORKING-DIR`
    * the current/working directory for the app
    * this will be the root path from which the website is generated
  * `VERSION`
    * the current version of the app
  * `LAST-UPDATED`
    * the time the app was last updated
    * based on the last write time of the [version file](../version)
  * `WEB-STATIC-DIR`
    * the directory containing static client-side web resources

```lisp
(defstruct app
  "Contains general, high-level app details."
  (name "" :type STRING)
  (app-dir nil :type PATHNAME)
  (working-dir nil :type PATHNAME)
  (version "0.0" :type STRING)
  (last-updated nil :type TIMESTAMP)
  (web-static-dir nil :type PATHNAME))


```

### CREATE-APP
* This function creates an instance of `APP`
  * with all fields properly initialised
* Note that `APP-VERSION` is loaded from a separate [version file](../version)
  * this is partly due to [fileworthy.asd](../fileworthy.asd) needing access to the app version as well
  * and this way we have a single place where the version gets updated
    * and it's easily modified and read from

```lisp
(defun create-app ()
  "Create APP instance."
  (let ((app-dir (asdf:system-source-directory :fileworthy))
        (working-dir (get-pathname-defaults))
        (version-file-path (asdf:system-relative-pathname
                            :fileworthy
                            "version")))
    (make-app :name
              (last1 
                (split-sequence
                  #\/
                  (princ-to-string (uiop/filesystem:truename* working-dir))
                  :remove-empty-subseqs t))
              :app-dir app-dir 
              :working-dir working-dir
              :version (asdf::read-file-form version-file-path)
              :last-updated
              (universal-to-timestamp
                (file-write-date version-file-path))
              :web-static-dir (merge-pathnames #P"static/" app-dir))))


```

## Startup and Shutdown

* To launch the website with the default values we need only call `START-APP`

### `START-APP`

* This function starts the app
  * including the underlying web server
  * by default at http://localhost:9090
* Parameters:
  * `PORT`
    * the port of the web server
  * `DEBUG`
    * whether to start the web server in debug mode where:
      * errors are caught by the debugger
      * errors are shown in HTML output
        * rather than showing a friendly "server error" page
* Returns:
  * an `R` result
* Side-effects:
  * sets `*ACCEPTOR*` and `*APP*`
  * since the Hunchentoot web server is started
    * this may initialise its own global variables
    * and produce it's own side-effects
    * this function explicitly sets the following Hunchentoot global vars:
      * `*CATCH-ERRORS-P*`
      * `*SHOW-LISP-ERRORS-P*`
  * calls `DEFINE-ROUTES`
    * which has its own side-effects

```lisp
(defun start-app (&key (port 9090) (debug t))
  "Starts the app."

  (if *acceptor*
    (let* ((res (new-r :error "Server is already running.")))
      (format t (r-message res))
      (return-from start-app res)))

  (setf *app* (create-app))

  (setf *acceptor* (create-web-acceptor :port port :debug debug))

  (define-routes)

  (when debug
    (setf *catch-errors-p* nil)
    (setf *show-lisp-errors-p* t))

  (start *acceptor*)

  (let* ((res (new-r :success
                     (sf "Fileworthy ~A started on port ~A, working out of '~A'."
                         (app-version *app*) port (app-working-dir *app*)))))
    (format t (r-message res))
    res))


```

### `STOP-APP`

* This function gracefully shuts down the app
  * including the underlying web server

```lisp
(defun stop-app ()
  "Stops the app."
  (when *acceptor*
    (stop *acceptor* :soft t)
    (setf *acceptor* nil)
    (format t "Stopped Fileworthy ~A~%" (app-version *app*))))


```

### `RESTART-APP`

* This function restarts the app
  * including the underlying web server

```lisp
(defun restart-app ()
  "Restarts the app."
  (stop)
  (start))



```

### `CREATE-WEB-ACCEPTOR`

* This function creates the Hunchentoot (easy) acceptor
* See `START-APP` for a description of the parameters
  * as it uses the exact same list

```lisp
(defun create-web-acceptor (&key (port 9090) (debug t))
  "Creates an 'easy-acceptor' which will listen on the specified port."
  (make-instance 'easy-acceptor
                 :port port
                 :document-root (app-web-static-dir *app*)
                 :access-log-destination (if debug
                                           *standard-output*
                                           "tbnl-access.log")
                 :message-log-destination (if debug
                                            *standard-output*
                                            "tbnl-message.log")))


```

## Core Domain Logic

### `GET-DIR-NAMES`

* This function gets a list of directory names relative to either
  * the given directory, `PARENT`
  * or the root working folder as specified by `APP-WORKING-DIR`

```lisp
(defun get-dir-names (&optional parent)
  "Get directory names."
  (map 'list
       (λ (abs-dir)
          (last1 (split-sequence #\/
                                 (princ-to-string abs-dir)
                                 :remove-empty-subseqs t)))
       (uiop/filesystem:subdirectories (or parent (app-working-dir *app*)))))

```

### `GET-FILE-NAMES`

* This function gets a list of file names relative to either
  * the given directory `PARENT`
  * or the root working folder as specified by `APP-WORKING-DIR`

```lisp
(defun get-file-names (&optional parent)
  "Get file names."
  (map 'list
       (λ (abs-file)
          (last1 (split-sequence #\/
                                 (princ-to-string abs-file)
                                 :remove-empty-subseqs t)))
       (uiop/filesystem:directory-files
         (or parent (app-working-dir *app*)))))


```

### `GET-FILE-CONTENT`

```lisp
(defun get-file-content (path)
  "Get file contents of `PATH`."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


```

### `IS-FILE-BINARY?`

* This function attempts to determine whether the given path is a binary file
* It does this with a very simple technique of looking for a 0 byte
* This technique should work with ASCII and UTF-8 files
  * but not UTF-16 and UTF-32

```lisp
(defun is-file-binary? (path)
  "Try to detect if PATH is a binary file."
  (with-open-file (stream path
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist nil)
    (loop :for i :from 0
          :for b = (read-byte stream nil nil)
          :while b
          :when (zerop b)
          :do (return-from is-file-binary? t)))
  nil)


```

## Web Utils

* This section contains utility functions common to most web functionality

```lisp

(defun set-http-code (code)
  "Set the current request's HTTP status code to `CODE`."
  (setf (return-code*) code))


```

## Web Resource Routes

* Route are defined in a function since
  * we need to have the `*APP*` instance initialised first
  * the page functions are defined below this point

```lisp
(defun define-routes ()
  "Define web resource routes."
  (setq *dispatch-table*
        (list
          ;; Static files
          (create-folder-dispatcher-and-handler
            "/css/"
            (merge-pathnames* "css/" (app-web-static-dir *app*)))
          (create-folder-dispatcher-and-handler
            "/deps/"
            (merge-pathnames* "deps/" (app-web-static-dir *app*)))
          (create-folder-dispatcher-and-handler
            "/js/"
            (merge-pathnames* "js/" (app-web-static-dir *app*)))

          ;; File-system path page
          (create-regex-dispatcher "^/*" #'page-fs-path))))


```

## Web Pages

* All web page-related functions will be prefixed with `PAGE-`

### `PAGE-TEMPLATE`

* This function defines the template all pages will use
* Parameters:
  * `TITLE`
    * the title of the page
    * note that the given title is
      * suffixed with the site's name and project name (Fileworthy)
      * and separated by a hyphen
      * e.g. "Home - My Documents - Fileworthy"
  * `CONTENT`
    * the HTML of the page as a raw string
    * note that the caller is responsible for properly escaping special characters

```lisp
(defun page-template (title content)
  "Base template for all web pages."
  (let* ((path-name (script-name* *request*))
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

             (:link :href "/images/favicon.ico" :rel "shortcut icon")
             (:link
               :href "/deps/font-awesome/css/font-awesome.min.css"
               :rel "stylesheet"
               :type "text/css")
             (:link
               :href "/deps/highlightjs/styles/github.css"
               :rel "stylesheet")
             (:link :href "/css/main.css" :rel "stylesheet")

             (:script :src "/deps/lodash/lodash.min.js" "")
             (:script :src "/deps/momentjs/moment.min.js" "")
             (:script :src "/deps/markedjs/marked.min.js" "")
             (:script :src "/deps/highlightjs/highlight.pack.js" ""))
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
                  (:a :href "/" :title "Home"
                   (:i :class "fa fa-home" "")))
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
                                dir-name)))))
               ;; Sub-folders
               (let* ((expanded-dirs (expand-sub-dirs path-name))
                      (sub-dir-name-lst (map 'list
                                             (λ (sub-dir)
                                                (get-dir-names sub-dir))
                                             expanded-dirs)))
                 (loop :for sub-dir-names :in sub-dir-name-lst
                       :for i :from 0
                       :when (non-empty? sub-dir-names) 
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


```


* An example call of the following is:
  * "root/sub1/sub1a" ==> '("root", "root/sub1", "root/sub1/sub1a")

```lisp
(defun expand-sub-dirs (path-name)
  "Expand all the path segments in PATH-NAME to a list of sub-directories."
  (let* ((path-name (string-trim '(#\/) (or path-name ""))))
    (if (empty? path-name)
      (return-from expand-sub-dirs nil))
    (loop :for c :across path-name
          :for i :from 0
          :when (char= #\/ c)
          :collect (subseq path-name 0 i) :into lst
          :finally (return (append lst (list path-name))))))

```

### Error pages

```lisp
(defun page-error-not-found ()
  "Not found error page."
  (set-http-code +http-not-found+)
  (page-template
    "Not Found"
    (markup
      (:h2 "Not Found")
      (:p "The page or resource you requested could not be found.")
      (:p
        (:a :href "/"
         (:i :class "fa fa-home" "")
         (:b " Go back to the home page"))))))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 404)) &key)
  (page-error-not-found))

(defun page-error-server ()
  "Internal server error page."
  (set-http-code +http-internal-server-error+)
  (page-template
    "Server Error"
    (markup
      (:h2 "Server Error")
      (:p (sf '("Sorry, it looks like something went wrong on the server. "
                "Please try again later if the problem persists.")))
      (:p
        (:a :href "/"
         (:i :class "fa fa-home" "")
         (:b " Go back to the home page"))))))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 500)) &key)
  (page-error-server))

```

### `PAGE-FS-PATH`

* This page displays a file-system path
  * i.e. a directory or file
* If the file appears to be a binary file, don't show it but provide links with options
* If the path is a directory with one non-binary file in it, just show it

```lisp
(defun page-fs-path ()
  "File-system path page."
  (let* ((path-name (script-name* *request*))
         (path-segs (split-sequence #\/ path-name :remove-empty-subseqs t))
         (abs-fs-path (get-fs-path-from-url path-name))
         (dir-exists? (if (non-empty? abs-fs-path)
                        (directory-exists-p abs-fs-path)))
         (file-exists? (if (and (not dir-exists?)
                                (non-empty? abs-fs-path))
                         (file-exists-p abs-fs-path)))
         (binary-file? nil)
         (loaded-file-name "")
         (rel-fs-path (if abs-fs-path
                        (subpathp abs-fs-path (app-working-dir *app*))))
         (file-content "")
         (file-names (get-file-names abs-fs-path)))
    ;; Show 404 page if dir/file not found
    (if (and (null dir-exists?) (null file-exists?))
      (return-from page-fs-path (page-error-not-found)))
    ;; Download file
    (if (and (get-parameter "download")
             file-exists?)
      (return-from page-fs-path (handle-static-file abs-fs-path)))
    ;; File requested
    (when file-exists?
      (setf binary-file? (is-file-binary? abs-fs-path))
      (when (not binary-file?)
        (setf loaded-file-name (last1 path-segs))
        (setf file-content (get-file-content abs-fs-path))))
    ;; Directory requested, but only one file in dir so show it
    (when (and dir-exists? (= 1 (length file-names)))
      (setf abs-fs-path (concatenate 'string
                                     (to-string abs-fs-path)
                                     (first file-names)))
      (setf binary-file? (is-file-binary? abs-fs-path))
      (when (not binary-file?)
        (setf loaded-file-name (first file-names))
        (setf file-content (get-file-content abs-fs-path))))
    (page-template
      (if (empty? rel-fs-path) "Home" rel-fs-path)
      (markup
        (:ul :id "files" :class "file-names"
         (loop
           :for file-name :in file-names
           :collect
           (markup
             (:li
               (:a
                 :class "download"
                 :href
                 (sf "~A?download" file-name)
                 (:i :class "fa fa-download" ""))
               (:a
                 :class
                 (if (string= file-name loaded-file-name)
                   "selected"
                   nil)
                 :href file-name
                 file-name)))))
        (:p
          (:i
            :class
            (if dir-exists?
              "fa fa-folder-open"
              "fa fa-file")
            "")
          (:span " ")
          (:span (if (empty? rel-fs-path)
                   "/"
                   (to-string rel-fs-path))))
        (:section :id "file-details"
         (if binary-file?
           (raw
             (markup
               (:p "This appears to be a binary file, and so can't be displayed here.")
               (:p
                 "You can "
                 (:a :href (sf "~A?download" loaded-file-name) "download the file")
                 " instead.")))
           (raw (markup
                  (:pre
                    (:code :id "raw-file-content" :class "hidden" file-content))
                  (:div :id "gen-file-content")))))))))

(defun get-fs-path-from-url (path-name)
  "Gets an absolute local file-system path from the given path name."
  (let* ((path (merge-pathnames* (string-left-trim '(#\/) path-name))))
    (if (subpathp path (app-working-dir *app*))
      path)))

```
