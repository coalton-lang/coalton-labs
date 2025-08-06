(defpackage #:flime/swank/shell
  (:use #:cl)
  (:export #:launch-lisp))
(in-package #:flime/swank/shell)

;;
;; Derived from "qlot/utils/shell" in https://github.com/fukamachi/qlot.

(defun safety-background-command (program args &key input output)
  (setf args (mapcar #'princ-to-string args))
  (uiop:launch-program (cons program args)
                       :input input
                       :output output
                       :error-output :interactive))

(defvar *current-lisp-path*
  (or #+ccl (car ccl:*command-line-argument-list*)
      #+sbcl (car sb-ext:*posix-argv*)
      #+allegro (car (system:command-line-arguments))
      #+clisp "clisp"
      #+cmu (car ext:*command-line-strings*)
      #+(or ecl clasp) (si:argv 0)
      #+abcl "java"))

(defvar *eval-option*
  (or
   #+ros.init "-e"
   #+ccl "--eval"
   #+sbcl "--eval"
   #+allegro "-e"
   #+clisp "-x"
   #+cmu "-eval"
   #+ecl "-eval"
   #+clasp "-e"
   #+abcl "--eval"))

(defun str (form)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (let ((*package* (find-package :cl-user)))
    (if (stringp form)
        form
        (let ((*print-case* :downcase) (*print-pretty* nil))
          (prin1-to-string form)))))

(defun -e (form)
  (list *eval-option* (str form)))

(defun default-args ()
  (append
   (-e "(require 'asdf)")))

(defun build-command-args (forms &key systems source-registry)
  (append
   (default-args)

   (when source-registry
     (-e `(push ,source-registry asdf:*central-registry*)))

   (loop for system in systems
         append (-e
                 `(if (find :quicklisp *features*)
                      (uiop:symbol-call :ql :quickload ,system :silent t)
                      (let ((*standard-output* (make-broadcast-stream))
                            (*error-output* (make-broadcast-stream))
                            (*trace-output* (make-broadcast-stream)))
                        (asdf:load-system ,system)))))

   (loop for form in forms
         append (-e
                 (if (pathnamep form)
                     `(load ,form)
                     form)))))

#+ros.init
(defun precommand-options ()
  '())

#-ros.init
(defun precommand-options ()
  #+abcl `("-jar" ,(uiop:native-namestring
                    (first (pathname-device ext:*lisp-home*))))

  #+ccl '("--no-init" "--quiet" "--batch")
  #+sbcl '("--noinform" "--no-sysinit" "--no-userinit" "--non-interactive")
  #+allegro '("--qq")
  #+clisp '("-norc" "--quiet" "--silent" "-on-error" "exit")
  #+cmu '("-noinit")
  #+ecl '("-norc")
  #+clasp '("--noinform" "--norc" "--non-interactive")
  #+abcl '("--noinform" "--noinit"))

#+ros.init
(defun postcommand-options ()
  '("run"))

#-ros.init
(defun postcommand-options ()
  (-e
   (quote
    #+ccl (ccl:quit)
    #+sbcl (sb-ext:exit)
    #+allegro (excl:exit :quiet t)
    #+clisp (ext:quit)
    #+cmucl (unix:unix-exit)
    #+ecl (ext:quit)
    #+abcl (ext:quit)
    #-(or ccl sbcl allegro clisp cmucl ecl abcl) (cl-user::quit))))

(defun command-options (forms args)
  (append
   (precommand-options)
   (apply #'build-command-args forms args)
   (postcommand-options)))

(defun launch-lisp (forms &rest args &key systems source-registry)
  (declare (ignore systems source-registry))
  (safety-background-command
   #-ros.init *current-lisp-path*
   #+ros.init (or (ros:opt "wargv0")
                  (ros:opt "argv0"))
   (command-options forms args)
   :input :stream
   :output :stream))
