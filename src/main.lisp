(in-package :fileworthy)

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
  "Singleton instance containing config details.")

(defvar *acceptor*
  nil
  "Singleton Hunchentoot web handler.")

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
