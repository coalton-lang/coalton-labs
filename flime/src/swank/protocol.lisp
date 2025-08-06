(defpackage #:flime/swank/protocol
  (:use #:cl)
  (:shadow #:debug)
  (:import-from #:flime/swank/connection
                #:connection
                #:connection-lock
                #:connection-socket
                #:connection-continuation-counter
                #:connection-rex-continuations)
  (:import-from #:babel
                #:string-to-octets
                #:octets-to-string)
  (:import-from #:swank)
  (:import-from #:alexandria
                #:when-let
                #:destructuring-case
                #:destructuring-ecase)
  (:export #:send-message
           #:receive-message
           #:swank-send
           #:swank-rex-async
           #:swank-rex
           #:make-dispatch-event-function))
(in-package #:flime/swank/protocol)

(deftype octet ()
  '(unsigned-byte 8))

(deftype octets (&optional length)
  `(simple-array (unsigned-byte 8) (,(or length '*))))

(defun encode-length (n)
  (format nil "~6,'0,X" n))

(defun decode-length (length-string)
  (parse-integer length-string :radix 16))

(define-condition swank-network-error (error) ())

(defvar *io-package*
  (let ((package (make-package :flime-swank-protocol-io-package :use '())))
    (import '(nil t quote) package)
    package))

(defun write-data-to-socket (data socket)
  (check-type data octets)
  (check-type socket usocket:stream-usocket)
  (let* ((body-length (1+ (length data)))
         (length-payload (string-to-octets (encode-length body-length)))
         (payload (make-array (+ 6 body-length) :element-type 'octet)))
    (replace payload length-payload)
    (replace payload data :start1 6)
    (setf (aref payload (1- (length payload))) (char-code #\Newline))
    (let ((stream (usocket:socket-stream socket)))
      (handler-bind
          ((error
             (lambda (e)
               (declare (ignore e))
               (error 'swank-network-error))))
        (write-sequence payload stream)
        (force-output stream)
        t))))

(defun send-message (sexp connection)
  (check-type connection connection)
  (bt:with-recursive-lock-held ((connection-lock connection))
    (let ((sexp-string (with-standard-io-syntax
                         (let ((*package* *io-package*)
                               (*print-case* :downcase))
                           (prin1-to-string sexp)))))
      (write-data-to-socket (string-to-octets sexp-string)
                            (connection-socket connection)))))

(defun read-n-bytes-from-socket (socket n-bytes)
  (let ((stream (usocket:socket-stream socket))
        (data (make-array n-bytes :element-type 'octet))
        (read-bytes 0))
    (loop
      (let ((bytes (handler-case
                       (read-sequence data stream :start read-bytes)
                     (error ()
                       (return)))))
        (incf read-bytes bytes)
        (when (= read-bytes n-bytes)
          (return data))
        (usocket:wait-for-input socket)))))

(defun read-message-length (socket)
  (when-let (length-data (read-n-bytes-from-socket socket 6))
    (decode-length (octets-to-string length-data))))

(defun read-data-from-socket (socket)
  (check-type socket usocket:stream-usocket)
  (when-let (body-length (read-message-length socket))
    (read-n-bytes-from-socket socket body-length)))

(defun receive-message (connection)
  (when-let (data (read-data-from-socket (connection-socket connection)))
    (let ((message-string (octets-to-string data)))
      (with-standard-io-syntax
        (let ((*package* *io-package*)
              (*print-case* :downcase))
          (handler-bind ((error
                           (lambda (e)
                             (when-let ((restart (find-restart 'unintern e)))
                               (invoke-restart restart)))))
            (let ((message (read-from-string message-string)))
              message)))))))

(defun swank-send (message connection)
  (bt:with-recursive-lock-held ((connection-lock connection))
    (send-message message connection)))

(defun swank-rex-async (form connection &key (package "COMMON-LISP-USER") continuation (thread 't))
  (let ((message
          (bt:with-recursive-lock-held ((connection-lock connection))
            (let ((id (incf (connection-continuation-counter connection))))
              (when continuation
                (push
                 (cons id continuation)
                 (connection-rex-continuations connection)))
              `(:emacs-rex ,form ,package ,thread ,id)))))
    (swank-send message connection)))

(defun swank-rex (form connection &rest args &key package thread)
  (declare (ignore package thread))
  (let ((condvar (bt:make-condition-variable))
        (condlock (bt:make-lock))
        raw-message
        result-ready)
    (apply #'swank-rex-async form connection
           :continuation
           (lambda (message)
             (bt:with-lock-held (condlock)
               (setf raw-message message
                     result-ready t)
               (bt:condition-notify condvar)))
           args)
    (bt:with-lock-held (condlock)
      (loop until result-ready
            do (bt:condition-wait condvar condlock)))
    (destructuring-ecase raw-message
      ((:ok value)
       (values value t raw-message))
      ((:abort condition)
       (values condition nil raw-message)))))

;; TODO: Logging
(defun make-dispatch-event-function (connection)
  (lambda (event)
    (destructuring-case event
      ((:return value id)
       (bt:with-recursive-lock-held ((connection-lock connection))
         (when-let (rex-cont (assoc id (connection-rex-continuations connection)))
           (setf (connection-rex-continuations connection)
                 (remove rex-cont (connection-rex-continuations connection)))
           (funcall (cdr rex-cont) value))))
      ((t &rest rest)
       (declare (ignore rest))))))
