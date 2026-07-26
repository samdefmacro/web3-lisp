;;;; Persistent Swank development image for web3-lisp.
;;;;
;;;; Started by scripts/dev.sh (which passes --dynamic-space-size 4096 —
;;;; Coalton compilation exhausts the default heap). Loads the full web3
;;;; system plus tests, then serves Swank on loopback.

(require :asdf)

(defun getenv/default (name default)
  (or (uiop:getenv name) default))

(let* ((here (uiop:ensure-directory-pathname (uiop:getcwd)))
       (quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
       (port (parse-integer (getenv/default "DEV_SWANK_PORT" "4008"))))
  (unless (probe-file quicklisp-setup)
    (error "Quicklisp setup file not found at ~A" quicklisp-setup))
  (load quicklisp-setup)
  (pushnew here asdf:*central-registry* :test #'equal)
  (funcall (find-symbol "QUICKLOAD" "QL") '("swank") :silent t)
  (format t "~&;;; Loading web3/tests (Coalton compile — first run takes minutes)~%")
  (asdf:load-system "web3/tests")
  (format t "~&;;; Starting Swank on 127.0.0.1:~D~%" port)
  (funcall (find-symbol "CREATE-SERVER" "SWANK")
           :interface "127.0.0.1" :port port :dont-close t)
  (format t "~&;;; web3-lisp Swank dev image ready.~%")
  (loop (sleep 3600)))
