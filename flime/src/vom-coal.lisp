(defpackage #:vom-coal
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:import-from #:vom)
  (:import-from #:coalton)
  (:export #:emerg
           #:alert
           #:crit
           #:error
           #:warn
           #:notice
           #:info
           #:debug
           #:debug1
           #:debug2
           #:debug3
           #:debug4
           #:emerg*
           #:alert*
           #:crit*
           #:error*
           #:warn*
           #:notice*
           #:info*
           #:debug*
           #:debug1*
           #:debug2*
           #:debug3*
           #:debug4*

           #:LogLevel
           #:EmergLevel
           #:CritLevel
           #:ErrorLevel
           #:WarnLevel
           #:NoticeLevel
           #:InfoLevel
           #:DebugLevel
           #:Debug1Level
           #:Debug2Level
           #:Debug3Level
           #:Debug4Level)
  (:documentation "Wrapper for Vom, a tiny logging library written in Common Lisp."))
(in-package #:vom-coal)

(cl:defmacro define-logging-function (name)
  (cl:let ((message (cl:gensym "MESSAGE"))
           (args (cl:gensym "ARGS"))
           (name* (cl:intern (cl:format cl:nil "~A*" name))))
    `(coalton-toplevel
       (declare ,name (String -> Void))
       (define (,name ,message)
         (lisp Void (,message)
           (,(cl:intern (cl:string name) '#:vom) ,message)))
       (declare ,name* (String -> (List String) -> Void))
       (define (,name* ,message ,args)
         (,name
          (lisp String (,message ,args)
            (cl:apply #'cl:format cl:nil ,message ,args)))))))

(define-logging-function emerg)
(define-logging-function alert)
(define-logging-function crit)
(define-logging-function error)
(define-logging-function warn)
(define-logging-function notice)
(define-logging-function info)
(define-logging-function debug)
(define-logging-function debug1)
(define-logging-function debug2)
(define-logging-function debug3)
(define-logging-function debug4)

(coalton-toplevel
  (define-type LogLevel
    EmergLevel
    AlertLevel
    CritLevel
    WarnLevel
    NoticeLevel
    InfoLevel
    DebugLevel
    Debug1Level
    Debug2Level
    Debug3Level
    Debug4Level)

  (define (config-all level)
    (match level
      ((EmergLevel)  (lisp Void () (vom:config cl:t :emerg)))
      ((AlertLevel)  (lisp Void () (vom:config cl:t :alert)))
      ((CritLevel)   (lisp Void () (vom:config cl:t :crit)))
      ((WarnLevel)   (lisp Void () (vom:config cl:t :warn)))
      ((NoticeLevel) (lisp Void () (vom:config cl:t :notice)))
      ((InfoLevel)   (lisp Void () (vom:config cl:t :info)))
      ((DebugLevel)  (lisp Void () (vom:config cl:t :debug)))
      ((Debug1Level) (lisp Void () (vom:config cl:t :debug)))
      ((Debug2Level) (lisp Void () (vom:config cl:t :debug)))
      ((Debug3Level) (lisp Void () (vom:config cl:t :debug)))
      ((Debug4Level) (lisp Void () (vom:config cl:t :debug))))
    Unit)

  (declare config-package-level (String -> LogLevel -> Void))
  (define (config-package-level package-name level)
    (match level
      ((EmergLevel)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :emerg)))
      ((AlertLevel)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :alert)))
      ((CritLevel)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :crit)))
      ((WarnLevel)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :warn)))
      ((NoticeLevel)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :notice)))
      ((InfoLevel)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :info)))
      ((DebugLevel)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :debug)))
      ((Debug1Level)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :debug1)))
      ((Debug2Level)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :debug2)))
      ((Debug3Level)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :debug3)))
      ((Debug4Level)
       (lisp Void (package-name)
         (vom:config (cl:intern package-name '#:keyword) :debug4))))))
