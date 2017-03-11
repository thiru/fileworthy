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
* [CL-WHO](http://weitz.de/cl-who/)
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
||#
(in-package :fileworthy)

#||
## APP

* The `APP` struct groups general, high-level app details including
  * `DEBUG`
    * whether the site is running in a debug mode
  * `APP-DIR`
    * the root directory of the app's source/binaries
  * `MIN-PASSWORD-LENGTH`
    * the minimum allowed password length
  * `VERSION`
    * the current version of the app
  * `LAST-UPDATED`
    * the time the app was last updated
    * based on the last write time of the [version file](../version)
  * `WEB-STATIC-DIR`
    * the directory containing static client-side web resources
  * `CONFIG-FILE-PATH`
    * the fully qualified path to the config file
    * which is an instance of `CONFIG`
||#
(defstruct app
  "Contains general, high-level app details."
  (debug t)
  (app-dir (empty 'pathname) :type PATHNAME)
  (min-password-length 4 :type INTEGER)
  (version "0.0" :type STRING)
  (last-updated (empty 'timestamp) :type TIMESTAMP)
  (web-static-dir (empty 'pathname) :type PATHNAME)
  (config-file-path (empty 'pathname) :type PATHNAME))

(empty=> (local-time:encode-timestamp 0 0 0 0 1 1 1))
(empty=> (make-app))

#||
### `CONFIG`

* This struct encapsulates user-configurable settings
  * `SITE-NAME`
    * the name for the website
  * `ROOT-DIR`
    * the root directory from which the website is generated
  * `ALLOW-ANONYMOUS-READ`
    * whether users that are not logged in are able to access the website
  * `RESERVED-RESOURCE-PATH`
    * the path within the site that is reserved for app-specific resources
    * essentially everything outside of a file-system path designation:
      * static files (javascript, css, etc)
      * admin pages
      * other custom pages
  * `NEXT-USER-ID`
    * holds the identifier that will be used for the next new user
  * `USERS`
    * all registered users
||#
(defstruct config
  (site-name "" :type STRING)
  (root-dir "" :type STRING)
  (port 0 :type INTEGER)
  (allow-anonymous-read t)
  (reserved-resource-path "" :type STRING)
  (next-user-id 1 :type INTEGER)
  (users '() :type LIST))

(empty=> (make-config))

;; TODO: expand environment variables in ROOT-DIR
(defun load-config (path)
  "Load an instance of `CONFIG` from the config file."
  (let* ((config (read-file-form path)))
    ;; If no root dir is specified, use user's home dir
    (if (blank? (config-root-dir config))
      (setf (config-root-dir config)
            (to-string (uiop/common-lisp:user-homedir-pathname))))
    (if (not (char-equal #\/
                         (char (config-root-dir config)
                               (1- (length (config-root-dir config))))))
      (setf (config-root-dir config)
            (sf "~A/" (config-root-dir config))))
    (if (not (directory-exists-p (config-root-dir config)))
      (error (sf "Directory '~A' not found or inaccessible."
                 (config-root-dir config))))
    config))

#||
## Global Variables

* I'm trying to keep the number of global objects as small as possible
* All of the following will be initialised when `START-APP` is called
  * so we don't bother initialising them here
||#
(defvar *app*
  (empty 'app)
  "Singleton instance containing general app details.")

(defvar *config*
  (empty 'config)
  "Singleton instance contain config details.")

(defvar *acceptor*
  nil
  "Singleton Hunchentoot web handler.")

#||
### CREATE-APP
* This function creates an instance of `APP`
  * with all fields properly initialised
* Note that `APP-VERSION` is loaded from a separate [version file](../version)
  * this is partly due to [fileworthy.asd](../fileworthy.asd) needing access to the app version as well
  * and this way we have a single place where the version gets updated
    * and it's easily modified and read from
||#
(defun create-app (debug)
  "Create APP instance."
  (let* ((app-dir (asdf:system-source-directory :fileworthy))
         (version-file-path (asdf:system-relative-pathname
                              :fileworthy
                              "version"))
         (xdg-config-home (xdg-config-home))
         (config-file-name "config")
         (config-file-dir "")
         (config-file-path ""))

    ;; If $XDG_CONFIG_HOME not set, set it to ~/.config
    (if (empty? xdg-config-home)
      (setf xdg-config-home (merge-pathnames* ".config" (user-homedir-pathname))))

    (setf config-file-dir (merge-pathnames* "fileworthy/" xdg-config-home))
    (setf config-file-path (merge-pathnames* config-file-name config-file-dir))

    (when (not (directory-exists-p config-file-dir))
      (format t
              "Creating '~A' as it doesn't exist.~%"
              config-file-dir)
      (ensure-directories-exist config-file-dir))

    (when (not (file-exists-p config-file-path))
      (format t
              "Creating '~A' from default config as it doesn't exist.~%"
              config-file-path)
      (copy-file (merge-pathnames* "config" app-dir) config-file-path))

    (make-app :debug debug
              :app-dir app-dir 
              :version (asdf::read-file-form version-file-path)
              :last-updated
              (universal-to-timestamp
                (file-write-date version-file-path))
              :web-static-dir (merge-pathnames #P"static/" app-dir)
              :config-file-path config-file-path)))

#||
## Startup and Shutdown

* To launch the website with the default values we need only call `START-APP`

### `START-APP`

* This function starts the app
  * including the underlying web server
  * by default at http://localhost:9090
* Parameters:
  * `PORT`
    * the port of the web server
    * if no value is specified here it is taken from the config, `CONFIG`
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
||#
(defun start-app (&key (port 9090 port-given?) (debug t))
  "Starts the app."

  (if *acceptor*
    (let* ((res (new-r :error "Server is already running.")))
      (format t (r-message res))
      (return-from start-app res)))

  (setf *app* (create-app debug))
  (setf *config* (load-config (app-config-file-path *app*)))

  (if (not port-given?)
    (setf port (config-port *config*)))

  (setf *acceptor* (create-web-acceptor :port port :debug debug))

  (define-routes)

  (when debug
    (setf *catch-errors-p* nil)
    (setf *show-lisp-errors-p* t))

  (start *acceptor*)

  (setf *session-max-time* (* 60 60 24 30 3)) ; 3 months

  (let* ((res (new-r :success
                     (sf "Fileworthy ~A started on port ~A, working out of '~A'."
                         (app-version *app*)
                         port
                         (config-root-dir *config*)))))
    (format t (r-message res))
    res))

#||
### `STOP-APP`

* This function gracefully shuts down the app
  * including the underlying web server
||#
(defun stop-app ()
  "Stops the app."
  (when *acceptor*
    (stop *acceptor* :soft t)
    (setf *acceptor* nil)
    (format t "Stopped Fileworthy ~A~%" (app-version *app*))))

#||
### `RESTART-APP`

* This function restarts the app
  * including the underlying web server
||#
(defun restart-app (&key (debug t))
  "Restarts the app."
  (stop-app)
  (start-app :debug debug))

#||
### `CREATE-WEB-ACCEPTOR`

* This function creates the Hunchentoot (easy) acceptor
* See `START-APP` for a description of the parameters
  * as it uses the exact same list
||#
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

#||
## Core Domain Logic

### `USER`

* This struct encapsulates a user account
||#
(defstruct user
  (id 0 :type INTEGER)
  (name "" :type STRING)
  (email "" :type STRING)
  (root-dir "" :type STRING) 
  (password "" :type STRING)
  (salt "" :type STRING)
  (admin? nil :type BOOLEAN))

(empty=> (make-user))

(defun get-user (&key (id 0 id-given?) name email)
  "Get user with one or more of the specified parameters."
  (if (or id-given? name email)
    (find-if (λ (user)
                (and (or (not id-given?) (= id (user-id user)))
                     (or (null name) (string-equal name (user-name user)))
                     (or (null email) (string-equal email (user-email user)))))
             (config-users *config*))))

(defun get-abs-user-root-dir (user)
  "Get the absolute root directory for the specified user."
  (if user
    (let* ((abs-dir (config-root-dir *config*)))
      (if (not (blank? (user-root-dir user)))
        (setf abs-dir
              (sf "~A~A/"
                  abs-dir
                  (string-trim '(#\/) (user-root-dir user)))))
      abs-dir)))

(defun authenticate-user (user pwd)
  "Authenticate the given user."
  (and user
       (not (blank? pwd))
       (string= (gen-hash pwd (user-salt user))
                (user-password user))))

#||
### `RANDOM-STRING`

* The Iron Clad function is wrapped in `TO-STRING` so that it prints like a
  regular string (e.g. used when saving config to disk)
  * otherwise the printed representation has "COERCE..."
||#
(defun random-string (&optional (n 16))
  "Return a random hex string with N digits."
  (to-string (ironclad:byte-array-to-hex-string (ironclad:make-random-salt n))))

#||
### `GEN-HASH`

* The Iron Clad function is wrapped in `TO-STRING` so that it prints like a
  regular string (e.g. used when saving config to disk)
  * otherwise the printed representation has "COERCE..."
||#
(defun gen-hash (to-hash &optional salt)
  "Generate a hash of TO-HASH."
  (to-string
    (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence :sha512
                                (ironclad:ascii-string-to-byte-array
                                  (sf "~A~A" to-hash (or salt "")))))))

#||
### `GET-FS-PATH-FROM-URL`
||#
(defun get-fs-path-from-url (user &optional path-name)
  "Gets an absolute local file-system path from the given URL path name."
  (let* ((abs-path (get-abs-user-root-dir user)))
    (if (not (blank? path-name))
      (setf abs-path (join abs-path (trim path-name #\/ :left-only t))))
    abs-path))

#||
### `GET-DIR-NAMES`

* This function gets a list of directory names relative to either
  * the given directory, `PARENT`
  * or the root working folder as specified by `CONFIG-ROOT-DIR`
    * and possibly further restricted by the user's root dir: `USER-ROOT-DIR`
||#
(defun get-dir-names (curr-user &optional (parent ""))
  "Gets directory names."
  (map 'list
       (λ (abs-dir)
          (last1 (split-sequence #\/
                                 (princ-to-string abs-dir)
                                 :remove-empty-subseqs t)))
       (uiop/filesystem:subdirectories
         (concatenate 'string
                      (config-root-dir *config*)
                      (string-trim '(#\/) (user-root-dir curr-user))
                      "/"
                      (string-left-trim '(#\/) parent)))))
#||
### `GET-FILE-NAMES`
||#
(defun get-file-names (dir)
  "Gets a list of file names at `DIR`."
  (map 'list
       (λ (abs-file)
          (last1 (split-sequence #\/
                                 (princ-to-string abs-file)
                                 :remove-empty-subseqs t)))
       (uiop/filesystem:directory-files dir)))

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
### `PRETTY-TIME`
||#
(defun pretty-time (time)
  "Formats a date/time to a user-friendly form. TIME is expected to either be a
   timestamp readable by LOCAL-TIME, or a LOCAL-TIME:TIMESTAMP object."
  (if (empty? time)
      ""
      (let* ((format-desc '())
             (timestamp (if (stringp time)
                            (parse-timestring time)
                            time)))

        (setf format-desc '(:short-weekday " " :short-month " " :day " "
                            :year ", " :hour12 ":" (:min 2) " " :ampm))

        (format-timestring nil timestamp :format format-desc))))
#||
### `IS-FILE-BINARY?`

* This function attempts to determine whether the given path is a binary file
* It does this with a very simple technique of looking for a 0 byte
* This technique should work with ASCII and UTF-8 files
  * but not UTF-16 and UTF-32
||#
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

#||
### `CREATE-CONFIG-LOCK-FILE`
||#
(defun create-config-lock-file ()
  "Create a file indicating that the config file is locked."
  (let* ((lock-file (sf "~A.lck" (app-config-file-path *app*))))
    (with-open-file (stream
                      lock-file
                      :direction :output
                      :if-exists nil
                      :if-does-not-exist :create)
      (if (null stream)
        (new-r :warning "Config file is already locked.")
        (progn
          (format stream "~A" (get-universal-time))
          (new-r :success "Config file lock created."))))))

#||
### `DELETE-CONFIG-LOCK-FILE
||#
(defun delete-config-lock-file ()
  "Delete config lock file."
  (delete-file-if-exists (sf "~A.lck" (app-config-file-path *app*))))

#||
### `SAVE-CONFIG`

* `CHANGE-FN` is a function that performs the work of making changes to the
  global `*CONFIG*` object
* This function is only called if the config file lock is successfully obtained
||#
(defun save-config (change-fn)
  "Save/update config file with contents of `*CONFIG*`."
  (let* ((lockedR (create-config-lock-file)))
    (if (failed? lockedR)
      (return-from save-config lockedR))
    (with-open-file (stream
                      (app-config-file-path *app*)
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
      (funcall change-fn)
      (write *config* :stream stream :readably t))
    (delete-config-lock-file))
  (new-r :success "Config updated."))

#||
## Web Utils

* This section contains common utility functions for the web-related code
||#

(defmacro gen-html (&body body)
  "Generate an HTML string from the given s-exp."
  `(with-html-output-to-string (*standard-output* nil)
    (htm ,@body)))

#||
### `SESSION-COOKIE-NAME`

* Let's use a custom session name
  * rather than the default: "hunchentoot-session"
||#
(defmethod session-cookie-name ((acceptor easy-acceptor))
  "fileworthy-session")

(defun set-http-code (code)
  "Set the current request's HTTP status code to `CODE`."
  (setf (return-code*) code))

(defun url-for (section-or-obj)
  "Create URL for a particular section/object"
  (cond ((eq 'about section-or-obj)
         (sf "/~A/about"
             (config-reserved-resource-path *config*)))
        ((eq 'settings section-or-obj)
         (sf "/~A/settings"
             (config-reserved-resource-path *config*)))
        ((eq 'users section-or-obj)
         (sf "/~A/users"
             (config-reserved-resource-path *config*)))
        ((typep section-or-obj 'user)
         (if (= 0 (user-id section-or-obj))
           (sf "/~A/users/new"
               (config-reserved-resource-path *config*))
           (sf "/~A/users/~A/~(~A~)"
               (config-reserved-resource-path *config*)
               (user-id section-or-obj)
               (user-name section-or-obj))))
        (t "")))

(defun json-result (result &optional data)
  "Converts the given R instance to a JSON string."
  (json:encode-json-plist-to-string
    `(level ,(r-level result)
            message ,(r-message result)
            data ,(or data (r-data result)))))

(defun json-error (status-code)
  "Create a JSON response indicating an error with the specified HTTP status
   code."
  (set-http-code status-code)
  (json:encode-json-plist-to-string
    '(level error
            message "Sorry, you don't have permission to perform this request.")))

(defun set-auth-cookie (name value)
  "Create a secure cookie."
  (set-cookie name
              :value value
              ;; Expire a month from now
              :max-age (* 60 60 24 30)
              :path "/"
              :secure (not (app-debug *app*))
              :http-only t))

(defun parse-js-bool (val)
  "Parse a Javascript boolean taken from a post parameter to a Lisp bool."
  (or (string-equal "true" val)
      (string-equal "1" val)))

#||
## Web Resource Routes

* Route are defined in a function since
  * we need to have the `*APP*` instance initialised first
  * the page functions are defined below this point
||#
(defun define-routes ()
  "Define web resource routes."
  (setq *dispatch-table*
        (list
          ;; Static files
          (create-folder-dispatcher-and-handler
            (sf "/~A/css/"
                (config-reserved-resource-path *config*))
            (merge-pathnames* "css/" (app-web-static-dir *app*)))
          (create-folder-dispatcher-and-handler
            (sf "/~A/deps/"
                (config-reserved-resource-path *config*))
            (merge-pathnames* "deps/" (app-web-static-dir *app*)))
          (create-folder-dispatcher-and-handler
            (sf "/~A/js/"
                (config-reserved-resource-path *config*))
            (merge-pathnames* "js/" (app-web-static-dir *app*)))

          ;; About page
          (create-regex-dispatcher
            (sf "^/~A/about/?$"
                (config-reserved-resource-path *config*))
            #'page-about)

          ;; Settings page
          (create-regex-dispatcher
            (sf "^/~A/settings/?$"
                (config-reserved-resource-path *config*))
            #'page-settings)

          ;; Settings save API
          (create-regex-dispatcher
            (sf "^/~A/api/settings/?$"
                (config-reserved-resource-path *config*))
            #'api-settings-save)

          ;; User list page
          (create-regex-dispatcher
            (sf "^/~A/users/?$"
                (config-reserved-resource-path *config*))
            #'page-user-list)

          ;; User detail page
          (create-regex-dispatcher
            (sf "^/~A/users/.+/?$"
                (config-reserved-resource-path *config*))
            #'page-user-detail)

          ;; User save API
          (create-regex-dispatcher
            (sf "^/~A/api/users/.+/?$"
                (config-reserved-resource-path *config*))
            #'api-user-save)

          ;; Login API
          (create-regex-dispatcher
            (sf "^/~A/api/login/?$"
                (config-reserved-resource-path *config*))
            #'api-login)

          ;; Logout page
          (create-regex-dispatcher
            (sf "^/~A/logout/?$"
                (config-reserved-resource-path *config*))
            #'page-logout)
          
          ;; File-system search API
          (create-regex-dispatcher
            (sf "^/~A/api/search/?$" (config-reserved-resource-path *config*))
            #'api-fs-search)

          ;; File-system path page (this should be the last entry)
          (create-regex-dispatcher
            "^/*"
            #'page-fs-path))))

#||
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
               :href (sf "/~A/deps/highlightjs/styles/github.css" rrp)
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
               :class (if fw-info-page?  "flat-list" "flat-list hidden")
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
                   :id "login-email-address"
                   :class "full-width"
                   :onkeyup "ui.onEnter(event, site.login)"
                   :placeholder "Email Address"
                   :title "Email Address"
                   :type "text"))
               (:p
                 (:input
                   :id "login-pwd"
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

#||
### Error pages
||#
(defun page-error-not-found ()
  "Not found error page."
  (set-http-code +http-not-found+)
  (page-template
    "Not Found"
    "not-found-page"
    (gen-html
      (:h2 "Not Found")
      (:p "The page or resource you requested could not be found.")
      (if (not (string-equal "/" (script-name* *request*)))
        (htm
          (:p
            (:a :href "/"
             (:i :class "fa fa-home" "")
             (:b " Go back to the home page"))))))))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 404)) &key)
  (page-error-not-found))

(defun page-error-not-authorised ()
  "Not authorised error page."
  (set-http-code +http-forbidden+)
  (page-template
    "Not Authorised"
    "not-authorised-page"
    (gen-html
      (:h2 "Not Authorised")
      (:p "Sorry, you don't have permission to view this page or resource.")
      (if (not (string-equal "/" (script-name* *request*)))
        (htm
          (:p
            (:a :href "/"
             (:i :class "fa fa-home" "")
             (:b " Go back to the home page"))))))))

(defun page-error-server ()
  "Internal server error page."
  (set-http-code +http-internal-server-error+)
  (page-template
    "Server Error"
    "server-error-page"
    (gen-html
      (:h2 "Server Error")
      (:p (sf '("Sorry, it looks like something went wrong on the server. "
                "Please try again later if the problem persists.")))
      (if (not (string-equal "/" (script-name* *request*)))
        (htm
          (:p
            (:a :href "/"
             (:i :class "fa fa-home" "")
             (:b " Go back to the home page"))))))))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 500)) &key)
  (page-error-server))

#||
### `PAGE-ABOUT`
||#
(defun page-about ()
  "About page."
  (page-template
    "About"
    "about-page"
    (gen-html
      (:h2 "About Fileworthy")
      (:p (sf '("Fileworthy aims to be a simple solution to managing your "
                "notes and files across many devices. It is half static site "
                "generator, half file-system.")))
      (:table :class "simple-table"
       (:tr
         (:td "Version")
         (:td (str (app-version *app*))))
       (:tr
         (:td "Last Updated")
         (:td (str (pretty-time (app-last-updated *app*)))))
       (:tr
         (:td "Source Code")
         (:td (:a :href "https://github.com/thiru/fileworthy"
               "Hosted at Github")))
       (:tr
         (:td "License")
         (:td
           (:a :href "https://www.gnu.org/licenses/gpl-3.0.html" "GPL v3")))
       (:tr
         (:td "Copyright")
         (:td "2017 Thirushanth Thirunavukarasu"))))))

#||
### `PAGE-SETTINGS`
||#
(defun page-settings ()
  "App settings page."
  (let* ((curr-user (session-value 'user)))
    ;; Only admins can view this page
    (if (or (null curr-user)
            (not (user-admin? curr-user)))
      (return-from page-settings (page-error-not-authorised)))
    (page-template
      "Settings"
      "settings-page"
      (gen-html
        (:h2 "Settings")
        (:ul :id "inputs" :class "flat-list"
         (:li
           (:label
             (:span "Site Name")
             (:input :id "site-name" :value (config-site-name *config*))
             (:div :class "clear-fix")))
         (:li
           (:label
             (:span "Root Folder")
             (:input :id "root-dir" :value (config-root-dir *config*))
             (:div :class "clear-fix")))
         (:li
           (:label
             (:span "Port")
             (:input :id "port" :value (to-string (config-port *config*)))
             (:div :class "clear-fix")))
         (:li
           (:label
             (:span "Allow anonymous read access")
             (if (config-allow-anonymous-read *config*)
               (htm
                   (:input :id "anon-read" :checked "" :type "checkbox"))
               (htm
                   (:input :id "anon-read" :type "checkbox")))
             (:div :class "clear-fix")))
         (:li
           (:label
             (:span "Reserved Resource Path")
             (:input
               :id "rrp"
               :value (config-reserved-resource-path *config*))
             (:div :class "clear-fix"))))
        (:div :id "save-result" "")
        (:button
          :id "save-btn"
          :class "button full-width"
          :onclick "page.save()"
          "Save")))))

#||
### `API-SETTINGS-SAVE`
||#
(defun api-settings-save ()
  "Settings save API."
  (setf (content-type*) "application/json")
  (let* ((curr-user (empty 'user :unless (session-value 'user)))
         (site-name (post-parameter "siteName"))
         (root-dir (post-parameter "rootDir"))
         (port (loose-parse-int (post-parameter "port")))
         (port-changed? (/= port (config-port *config*)))
         (anon-read? (parse-js-bool (post-parameter "anonRead")))
         (rrp (post-parameter "rrp"))
         (rrp-changed? (not (string= rrp
                                     (config-reserved-resource-path *config*))))
         (save-res (new-r :error "Settings save unexpectedly aborted.")))

    ;; Validation
    (if (or (empty? curr-user)
            (not (user-admin? curr-user))) 
      (return-from api-settings-save (json-error +http-forbidden+)))
    (if (not (plusp port))
      (return-from
        api-settings-save
        (json-result (new-r :error "Port must be a positive integer."))))
    (if (blank? rrp)
      (return-from
        api-settings-save
        (json-result (new-r :error "Reserved Resource Path is required."))))

    ;; Persist
    (setf save-res
          (save-config
            (λ ()
               (setf (config-site-name *config*) site-name)
               (setf (config-root-dir *config*) root-dir)
               (setf (config-port *config*) port)
               (setf (config-allow-anonymous-read *config*) anon-read?)
               (setf (config-reserved-resource-path *config*) rrp))))

    ;; Return success/failure
    (if (succeeded? save-res)
      (progn
        (setf *config* (load-config (app-config-file-path *app*)))
        (if rrp-changed?
          (define-routes))
        ;; TODO: automatically reset if the port changed
        (if port-changed?
          (json-result
            (new-r :success (sf '("Config updated. Please restart the app to "
                                  "use the new port."))))
          (json-result save-res)))
      (json-result save-res))))

#||
### `PAGE-USER-LIST`
||#
(defun page-user-list ()
  "User listing page."
  (let* ((curr-user (session-value 'user)))
    ;; Only admins can view this page
    (if (or (null curr-user)
            (not (user-admin? curr-user)))
      (return-from page-user-list (page-error-not-authorised)))
    (page-template
      "Users"
      "user-list-page"
      (gen-html
        (:a
          :id "new-user-btn"
          :class "button"
          :href (url-for (empty 'user))
          "New User")
        (:ul :class "big-list"
          (loop
            :for user :in (config-users *config*)
            :collect
            (htm
              (:li
                (:a
                  :href (url-for user)
                  (str (user-name user)))))))))))

#||
### `PAGE-USER-DETAIL`
||#
(defun page-user-detail ()
  "User details page."
  (let* ((curr-user (empty 'user :unless (session-value 'user)))
         (path-segs (split-sequence #\/ (script-name*) :remove-empty-subseqs t))
         (user-id-str (nth 2 path-segs))
         (new-user? (string-equal "new" user-id-str))
         (req-user (empty
                     'user
                     :unless (get-user :id (loose-parse-int user-id-str)))))
    ;; Redirect to Not Found page if user not found
    (if (and (not new-user?) (empty? req-user))
      (return-from page-user-detail (page-error-not-found)))
    ;; Redirect to Forbidden page if not admin and not current user
    (if (and (not (user-admin? curr-user))
             (not (eq curr-user req-user)))
      (return-from page-user-detail (page-error-not-authorised)))
    (page-template
      (if new-user? "New User" (user-name req-user))
      "user-detail-page"
      (gen-html
        (:h2
          :id "name-heading"
          :data-user-id (to-string (user-id req-user))
         (if new-user? "New User" (user-name req-user)))
        (:div :id "input-fields"
         (:input
           :id "user-name"
           :placeholder "Name"
           :title "Name"
           :type "text"
           :value (user-name req-user))
         (:input
           :id "email-address"
           :placeholder "Email Address"
           :title "Email Address"
           :type "email"
           :value (user-email req-user))
         (if (user-admin? curr-user)
           (htm
               (:input
                 :id "root-dir"
                 :placeholder "Root Folder"
                 :title "Root Folder"
                 :type "text"
                 :value (user-root-dir req-user))))
         (if (user-admin? curr-user)
           (htm
               (:label
                 (if (user-admin? req-user)
                   (htm
                       (:input :id "is-admin" :checked "" :type "checkbox"))
                   (htm
                       (:input :id "is-admin" :type "checkbox")))
                 " Administrator")))
         (:div
           :class (if new-user? "hidden" "")
           (:a
             :id "show-pwds-btn"
             :class "button"
             :href "javascript:page.toggleChangePwd()"
             "Change Password")
           (:a
             :id "hide-pwds-btn"
             :class "button hidden"
             :href "javascript:page.toggleChangePwd()"
             "Don't Change Password"))
         (:div
           :id "password-fields"
           :class (if new-user? "" "hidden")
           (:input
             :id "current-pwd"
             :class (if new-user? "hidden" "")
             :placeholder "Current Password"
             :title "Current Password"
             :type "password")
           (:input
             :id "new-pwd"
             :placeholder "New Password"
             :title "New Password"
             :type "password")
           (:input
             :id "new-pwd-confirm"
             :placeholder "Confirm New Password"
             :title "Confirm New Password"
             :type "password")))
        (:p :id "save-result" "")
        (:a
          :id "save-btn"
          :class "button full-width"
          :href "javascript:page.save()"
          "Save")))))

#||
### `API-USER-SAVE`
||#
(defun api-user-save ()
  "User save API."
  (setf (content-type*) "application/json")
  (let* ((curr-user (empty 'user :unless (session-value 'user)))
         (path-segs (split-sequence #\/ (script-name*) :remove-empty-subseqs t))
         (id (loose-parse-int (nth 3 path-segs)))
         (new-user? (zerop id))
         (req-user (empty 'user :unless (get-user :id id)))
         (name (post-parameter "name"))
         (email (post-parameter "email"))
         (root-dir (or (post-parameter "rootDir") ""))
         (admin? (and (user-admin? curr-user)
                      (parse-js-bool (post-parameter "isAdmin"))))
         (current-pwd (post-parameter "currentPwd"))
         (new-pwd (post-parameter "newPwd"))
         (save-res (new-r :error "User save unexpectedly aborted.")))

    ;; Validation
    (if (empty? curr-user)
      (return-from api-user-save (json-error +http-forbidden+)))
    (if (and (not new-user?) (empty? req-user))
      (return-from
        api-user-save
        (json-result (new-r :error "User with id ~A not found." id))))
    ;; Non-admins cannot change another user's password
    (if (and (not new-user?)
             (not (user-admin? curr-user))
             (/= (user-id curr-user) (user-id req-user)))
      (return-from
        api-user-save
        (json-error +http-forbidden+)))
    (if (empty? name)
      (return-from
        api-user-save
        (json-result (new-r :error "No user name provided."))))
    (if (empty? email)
      (return-from
        api-user-save
        (json-result (new-r :error "No email address provided."))))
    (if (and (empty? new-pwd)
             (or new-user?
                 (and (not new-user?) (not (empty? current-pwd)))))
      (return-from
        api-user-save
        (json-result (new-r :error "No password provided."))))
    (if (and (blank? new-pwd)
             (or new-user?
                 (and (not new-user?) (not (empty? current-pwd)))))
      (return-from
        api-user-save
        (json-result (new-r :error "Password can't be blank."))))
    (if (and (> (app-min-password-length *app*) (length new-pwd))
             (or new-user?
                 (and (not new-user?) (not (empty? current-pwd)))))
      (return-from
        api-user-save
        (json-result
          (new-r :error
                 (sf "Password must be at least ~A characters."
                     (app-min-password-length *app*))))))
    (if (and (not new-user?)
             (not (user-admin? curr-user))
             (not (empty? new-pwd))
             (not (authenticate-user req-user current-pwd)))
      (return-from
        api-user-save
        (json-result
          (new-r :error "Current password is incorrect."))))

    ;; Persist
    (setf save-res
          (save-config
            (λ ()
               (let* ((curr-config *config*)
                      (salt (random-string)))
                 (if new-user?
                   (progn
                     (push
                       (make-user
                         :id (config-next-user-id curr-config)
                         :name name
                         :email email
                         ;; Only admins can change a user's root dir
                         :root-dir (if (user-admin? curr-user)
                                     root-dir
                                     (user-root-dir req-user))
                         :admin? admin?
                         :salt salt
                         :password (gen-hash new-pwd salt))
                       (config-users curr-config))
                     (incf (config-next-user-id curr-config)))
                   (progn
                     (setf (user-name req-user) name)
                     (setf (user-email req-user) email)
                     (if (user-admin? curr-user)
                       (setf (user-root-dir req-user) root-dir))
                     (setf (user-admin? req-user) admin?)
                     (when (not (empty? new-pwd))
                       (setf (user-salt req-user) salt)
                       (setf (user-password req-user)
                             (gen-hash new-pwd salt)))))))))

    (if (failed? save-res)
      (return-from api-user-save (json-result save-res)))

    ;; If the password changed for an existing user, remove all session
    ;; objects for the user, except the current session
    (when (and (not new-user?)
               (not (empty? new-pwd)))
      (hunchentoot::with-session-lock-held
        ((session-db-lock *acceptor*))
        (setf (session-db *acceptor*)
              (loop :for (k . v) in (session-db *acceptor*)
                    :when (or (= (session-id v)
                                 (session-id *session*))
                              (/= (user-id req-user)
                                  (user-id (session-value 'user v))))
                    :collect (cons k v)))))

    ;; Return success
    (json-result (new-r :success
                        (if new-user?
                          (sf "Saved new user, ~A." name)
                          (sf "Updated ~A's account." name))))))
#||
### `API-LOGIN`
||#
(defun api-login ()
  "User login API."
  (setf (content-type*) "application/json")
  (let* ((email (post-parameter "email"))
         (pwd (post-parameter "pwd"))
         (user (get-user :email email)))
    (if (empty? email)
      (return-from
        api-login
        (json-result (new-r :error "No email address provided."))))
    (if (empty? pwd)
      (return-from
        api-login
        (json-result (new-r :error "No password provided."))))
    (when (not (authenticate-user user pwd))
      (sleep 2)
      (return-from
        api-login
        (json-result (new-r :error "Incorrect credentials."))))

    ;; Create session for user
    (setf (session-value 'user) user)

    (json-result (new-r :success (sf "Welcome ~A." (user-name user))))))

#||
### `PAGE-LOGOUT`
||#
(defun page-logout ()
  (when *session*
    (delete-session-value 'user)
    (remove-session *session*))
  (page-template
    "Logout"
    "logout-page"
    (gen-html
      (:h2 "Thank you, come again!")
      (:p
        (:a :class "full-width"
         :href "/"
         "Go back to Home page")))))

#||
### `API-FS-SEARCH`
||#
(defun api-fs-search ()
  "File-system search API."
  (setf (content-type*) "application/json")
  (let* ((user (empty 'user :unless (session-value 'user)))
         (user-root-dir-length (length (get-abs-user-root-dir user)))
         (search-type (post-parameter "search-type"))
         (search-txt (post-parameter "search"))
         (search-result nil))

    ;; Check anonymous access
    (if (and (empty? user)
             (not (config-allow-anonymous-read *config*)))
      (return-from api-fs-search (json-error +http-forbidden+)))

    (setf search-txt
          ;; Show all files if search text is "*"
          (if (string-equal "*" search-txt)
            ""
            ;; Maybe being overly cautious on allowed characters
            (ppcre:regex-replace-all "[^a-zA-Z0-9\\-_ \\./]+" search-txt "")))

    ;; Get search results (in absolute path form)
    (setf search-result
          ;; By default search for filenames rather than file content
          (if (string-equal "content" search-type)
            (search-file-content search-txt (get-abs-user-root-dir user))
            (search-file-names search-txt (get-abs-user-root-dir user))))

    ;; Trim absolute path segment
    (setf (r-data search-result)
          (map 'list
               (λ (x)
                  (if (empty? x)
                    x
                    (subseq x user-root-dir-length)))
               (r-data search-result)))

    (json-result search-result)))

(defun run-cmd (cmd)
  "Run command specified by `CMD'.
   A result object is returned."
  (multiple-value-bind (std-out std-err ret-val)
      (uiop:run-program cmd
                        :ignore-error-status t
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t))
    (if (zerop ret-val)
        (new-r :success "" std-out)
        (new-r :error
               (sf "ERROR ~A: ~A"
                   ret-val
                   (if (and (empty? std-out) (empty? std-err))
                       "unknown (cmd reported no info)"
                       (or std-err std-out)))))))

#||
#### `SEARCH-FILE-NAMES`

* This runs rg (ripgrep) to search for file names
* rg command arguments include
  * `--files`: only match pattern against filenames (i.e. not file content)
  * `--follow`: follow symlinks
  * `--glob`: include/exclude file pattern
  * `--ignore-case`: case insensitive search
* TODO: Add support for case-insensitive glob pattern matching
  * maybe this can be done by replacing every letter like so: "test" -> "[Tt][Ee][Ss][Tt]"
||#
(defun search-file-names (pattern &optional path)
  "Search for files matching `pattern` at `path`."
  (let* ((cmd (sf "rg --follow --ignore-case --glob '~A' --files ~A"
                  (if (empty? pattern) pattern (sf "*~A*" pattern))
                  (or path "")))
         (search-result nil))
    (log-message* :info "Filename search cmd: ~A" cmd)
    (setf search-result (run-cmd cmd))
    (if (succeeded? search-result)
      (setf (r-data search-result)
            (sort (split-sequence #\linefeed (r-data search-result))
                  #'string-lessp)))
    search-result))

#||
#### `SEARCH-FILE-CONTENT`

* This runs rg (ripgrep) to search for files containing some pattern
* rg command arguments include
  * `--files-with-matches`: only output filenames (i.e. don't output matching content within file)
  * `--fixed-strings`: treat `pattern` as a literal string instead of a regular expression
  * `--follow`: follow symlinks
  * `--glob`: include/exclude file pattern
  * `--ignore-case`: case insensitive search
  * `--text`: search binary files as if they were text
||#
(defun search-file-content (pattern &optional path)
  "Search for files containing text matching `pattern` within the `path`."
  (let* ((text-search-cmd
           (sf '("rg --files-with-matches --fixed-strings --follow"
                 " --ignore-case '~A' ~A")
               pattern
               (or path "")))
         (bin-search-cmd
           (sf '("rg --files-with-matches --fixed-strings --follow"
                 " --glob '*.{doc,docx,odt,pdf,ppt,pptx}' --ignore-case --text"
                 " '~A' ~A")
               pattern
               (or path "")))
         (text-search-result nil)
         (bin-search-result nil)
         (file-matches '()))

    ;; First search text files
    (log-message* :info "Text file content search cmd: ~A" text-search-cmd)
    (setf text-search-result (run-cmd text-search-cmd))
    (if (succeeded? text-search-result)
      (setf file-matches
            (split-sequence #\linefeed (r-data text-search-result))))
    (format t "TEXT-MATCHES: ~A~%" file-matches)

    ;; Then search binary files
    (log-message* :info "Binary file content search cmd: ~A" bin-search-cmd)
    (setf bin-search-result (run-cmd bin-search-cmd))
    (if (succeeded? bin-search-result)
      (setf file-matches
            (append file-matches
                    (split-sequence #\linefeed (r-data bin-search-result)))))

    ;; Join results and sort
    (if (empty? file-matches)
      (new-r :error
             (string-trim
               " "
               (concatenate 'string
                            (r-message text-search-result)
                            " "
                            (r-message bin-search-result))))
      (new-r :success "" (sort file-matches #'string-lessp)))))

#||
### `PAGE-FS-PATH`

* This page displays a file-system path
  * i.e. a directory or file
* If the file appears to be a binary file, don't show it but provide links with options
* If the path is a directory with one non-binary file in it, just show it
||#
(defun page-fs-path ()
  "File-system path page."
  (let* ((user (empty 'user :unless (session-value 'user)))
         (path-name (script-name* *request*))
         (path-segs (split-sequence #\/ path-name :remove-empty-subseqs t))
         (last-path-seg (last1 path-segs))
         (expanded-paths (expand-sub-dirs path-name))
         (abs-fs-path (empty 'string
                             :unless (get-fs-path-from-url user path-name)))
         (path-is-dir? (if (not (empty? abs-fs-path))
                         (directory-exists-p abs-fs-path)))
         (path-is-file? (if (and (not path-is-dir?)
                                 (not (empty? abs-fs-path)))
                         (file-exists-p abs-fs-path)))
         (binary-file? (if path-is-file? (is-file-binary? abs-fs-path)))
         (curr-file-name "")
         (file-content "")
         (file-names (get-file-names abs-fs-path))
         (dir-contains-index-file? (and file-names
                                        (find "index.md"
                                              file-names
                                              :test #'string-equal))))
    ;; Check anonymous access
    (if (and (empty? user)
             (not (config-allow-anonymous-read *config*)))
      (return-from page-fs-path (page-error-not-authorised)))
    ;; Show 404 page if dir/file not found
    (if (and (not path-is-dir?) (not path-is-file?))
      (return-from page-fs-path (page-error-not-found)))
    ;; Download file
    (if (and path-is-file?
             (or binary-file? (get-parameter "download")))
      (return-from page-fs-path (handle-static-file abs-fs-path)))
    ;; File requested
    (when path-is-file?
      (setf curr-file-name last-path-seg)
      (when (or (not binary-file?) (get-parameter "force-show"))
        (setf file-content (get-file-content abs-fs-path))))
    ;; Directory requested, but only one file in dir so show it
    (when (and path-is-dir? (= 1 (length file-names)))
      (setf abs-fs-path (concatenate 'string
                                     (to-string abs-fs-path)
                                     (first file-names)))
      (setf curr-file-name (first file-names))
      (when (or (not binary-file?) (get-parameter "force-show"))
        (setf file-content (get-file-content abs-fs-path))))
    ;; Directory requested and has "index.md" file, so show it
    (when (and path-is-dir? dir-contains-index-file?)
      (setf abs-fs-path (concatenate 'string
                                     (to-string abs-fs-path)
                                     "index.md"))
      (setf curr-file-name "index.md")
      (setf file-content (get-file-content abs-fs-path)))
    ;; TODO: fix JS injection
    (page-template
      (if (empty? last-path-seg) "Home" last-path-seg)
      "fs-path-page"
      (gen-html
        (:div :id "search-group"
          (:input
            :id "search"
            :autocomplete "off"
            :placeholder "Search page names"
            :onclick "page.onSearchTxtClick(event)"
            :onkeydown "page.onSearchTxtKeyDown(event)"
            :onkeyup "page.onSearchTxtKeyUp(event)")
          (:select
            :id "search-type"
            :onchange "page.onSearchTypeChange(event)"
            :title "Select the type of search to perform"
           (:option
             :data-long-text "Search page names"
             :value "page"
             "Pages")
           (:option
             :data-long-text "Search page content"
             :value "content"
             "Content")))
        (:div :id "search-info" "")
        (:select
          :id "search-results"
          :class "hidden"
          :data-default-size "10"
          :onclick "page.onSearchResultsClick(event)"
          :onkeydown "page.onSearchResultsKeyDown(event)"
          :size 10)
        (:table :id "files" :class "file-names"
         (:tbody
          (loop
           :for file-name :in file-names
           :collect
           (htm
             (:tr
               :class
               (if (string= file-name curr-file-name)
                 "selected"
                 nil)
               (:td
                 (:a
                   :href file-name
                   (str file-name)))
               (:td
                 (:a
                   :class "download"
                   :href (sf "~A?download" file-name)
                   :title "Download file"
                   (:i :class "fa fa-download" ""))))))))
        (if (>= (length path-segs) 2)
          (htm
            (:section :id "path-trail"
             (:span
               (:a :href "/"
                (:i :class "fa fa-folder-open" " "))
               " ")
             (loop :for path :in expanded-paths
                   :for i :from 0
                   :collect
                   (htm
                     (if (> i 0)
                       (htm
                         (:span "/")))
                     (:a
                       :href
                       (if (and path-is-file?
                                (= (1+ i) (length expanded-paths)))
                         (sf "/~A" path)
                         (sf "/~A/" path))
                       (str (nth i path-segs))))))))
        (:section :id "file-details"
         (if (or (not binary-file?) (get-parameter "force-show"))
           (let* ((is-markdown? (cl-ppcre:scan "\\.mk?d$" abs-fs-path))
                  (file-content (cl-ppcre:regex-replace-all
                                  "~"
                                  file-content
                                  "~~")))
             (if (not is-markdown?)
               (setf file-content (escape-string file-content)))
             (htm
               (:pre
                 (:code
                   :id "raw-file-content"
                   :class "hidden"
                   (write-string file-content)))
               (:div :id "gen-file-content")))
           (htm
             (:p "It looks like this is a binary file, so it isn't displayed.")
             (:p
               "You can "
               (:a
                 :href (sf "~A?download" curr-file-name)
                 "download the file")
               " or try to "
               (:a
                 :href (sf "~A?force-show" curr-file-name)
                 "display it anyway.")))))))))
