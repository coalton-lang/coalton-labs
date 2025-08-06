(defpackage #:flime/swank/server
  (:use #:cl)
  (:import-from #:flime/swank/shell
                #:launch-lisp)
  (:import-from #:usocket)
  (:export #:create-swank-server
           #:swank-server
           #:swank-server-process
           #:swank-server-host
           #:swank-server-port
           #:make-swank-server))
(in-package #:flime/swank/server)

(defun port-available-p (port)
  (let (socket)
    (unwind-protect
         (handler-case (progn
                         (setq socket (usocket:socket-listen "127.0.0.1" port :reuse-address t))
                         t)
           (usocket:address-in-use-error () nil)
           (usocket:socket-error (e)
             (warn "USOCKET:SOCKET-ERROR: ~A" e)
             nil))
      (when socket
        (usocket:socket-close socket)
        t))))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
        return port))

(defstruct swank-server
  process
  host
  port)

(defun server-running-p (port)
  (handler-case (let ((socket (usocket:socket-connect "127.0.0.1" port)))
                  (usocket:socket-close socket)
                  t)
    (usocket:connection-refused-error () nil)
    (usocket:connection-reset-error () nil)))

(defun create-swank-server (&key source-registry quicklisp-home port)
  (check-type source-registry list)
  (let ((port (or port (random-port))))
    (let ((process (launch-lisp
                    `(,@(and quicklisp-home
                             `((load (merge-pathnames #P"setup.lisp" ,quicklisp-home))))
                      (swank:create-server :port ,port :dont-close t))
                    :systems '("swank")
                    :source-registry source-registry)))
      (prog1
          (make-swank-server :process process
                             :host "127.0.0.1"
                             :port port)
        (loop repeat 300
              do (sleep 0.1)
              when (server-running-p port)
              do (return)
              unless (uiop:process-alive-p process)
              do (uiop:quit -1)
              finally
                 (uiop:quit -1))))))
