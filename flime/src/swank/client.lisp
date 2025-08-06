(defpackage #:flime/swank/client
  (:use #:cl)
  (:import-from #:flime/swank/server
                #:swank-server
                #:swank-server-host
                #:swank-server-port)
  (:import-from #:flime/swank/connection
                #:make-connection
                #:open-connection
                #:connection-process-thread)
  (:import-from #:flime/swank/protocol
                #:make-dispatch-event-function
                #:receive-message)
  (:import-from #:bordeaux-threads)
  (:export #:connect-to-swank-server
           #:start-processing))
(in-package #:flime/swank/client)

(defun make-process-thread (connection process-fn)
  (bt:make-thread
   (lambda ()
     (loop
       (let ((message (receive-message connection)))
         (when message
           (funcall process-fn message)))))
   :name "flime swank message processor"
   :initial-bindings
   `((*standard-output* . ,*standard-output*)
     (*error-output* . ,*error-output*)
     (bt:*default-special-bindings* . ',bt:*default-special-bindings*)
     ,@bt:*default-special-bindings*)))

(defun connect-to-swank-server (swank-server)
  (check-type swank-server swank-server)
  (let* ((host (swank-server-host swank-server))
         (port (swank-server-port swank-server))
         (conn (make-connection :host host
                                :port port)))
    (handler-case
        (open-connection conn)
      (usocket:connection-refused-error ()
        (uiop:quit -1)))
    conn))

(defun start-processing (connection)
  (setf (connection-process-thread connection)
        (make-process-thread connection
                             (make-dispatch-event-function connection))))
