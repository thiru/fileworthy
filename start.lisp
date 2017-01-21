;;;; Launches website with Swank

(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)

(ql:quickload :fileworthy)
(fileworthy:start-app :debug nil)
