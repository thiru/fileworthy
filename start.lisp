;;;; Starts the website with Swank in production mode

(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)

(declaim (optimize (safety 1) (speed 3)))

(ql:quickload :fileworthy)
(fileworthy:start-app :debug nil)

(loop (sleep 10))
