(defpackage #:flime/swank-ext/coalton
  (:use #:cl)
  (:import-from #:coalton)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker/environment)
   (#:entry #:coalton-impl/entry))
  (:export #:try-compile-file
           #:completions))
(in-package #:flime/swank-ext/coalton)

(defun try-compile-file (file-path)
  (let ((source (coalton-impl/source:make-source-file file-path)))
    (handler-case
        (progn
          (coalton-impl/entry:compile source :load nil)
          nil)
      (coalton-impl/source::source-condition (c)
        (with-open-stream (source-stream (coalton-impl/source::condition-stream c))
          (let ((state (coalton-impl/source::make-printer-state source-stream c)))
            (mapcar (lambda (note)
                      (let ((note (coalton-impl/source:message c))
                            (message (coalton-impl/source:message note))
                            (start
                              (coalton-impl/source::offset-position
                               state
                               (coalton-impl/source::start-offset note)))
                            (end
                              (coalton-impl/source::offset-position
                               state
                               (coalton-impl/source::end-offset note))))
                        ;; Return 6 values:
                        ;;   start-line, start-column, end-line, end-column, the condition note, and the error message.
                        (list (1- (car start))
                              (cdr start)
                              (1- (car end))
                              (cdr end)
                              note
                              message)))
                    (coalton-impl/source::notes c)))))
      ;; Fatal error
      ;; TODO: Handling and logging
      (error ()
        nil))))

(defun symbol-kind (symbol-name package-name)
  (check-type symbol-name string)
  (check-type package-name string)
  (let* ((package (find-package (string-upcase package-name)))
         (symbol (and package
                      (find-symbol (string-upcase symbol-name) package))))
    (unless symbol
      (error "Unknown symbol ~S in ~S" symbol-name package-name))
    (or (cdr
         (find-if (lambda (pair)
                    (destructuring-bind (fn . type)
                        pair
                      (when (funcall fn entry:*global-environment* symbol :no-error t)
                        type)))
                  `((tc:lookup-struct . "struct")
                    (tc:lookup-type . "class")
                    (tc:lookup-class . "class")
                    (tc:lookup-constructor . "constructor")
                    (tc:lookup-function . "function")
                    (tc:lookup-name . "variable"))))
        (and (macro-function symbol)
             "keyword"))))

;;
;; Context-aware auto-completion for Coalton, which is not completed. :P

(defun completions (prefix package-name)
  (mapcar (lambda (symbol-name)
            (let ((colon-pos (position #\: symbol-name)))
              (cons symbol-name
                    `(("kind"
                       . ,(if colon-pos
                              (symbol-kind (subseq symbol-name (1+ colon-pos))
                                           (subseq symbol-name 0 colon-pos))
                              (symbol-kind symbol-name package-name)))))))
          (first (swank:simple-completions prefix package-name))))
