(defpackage #:flime/swank-ext/common-lisp
  (:use #:cl
        #+sbcl #:flime/swank-ext/common-lisp/sbcl)
  (:export #:try-compile-file))
(in-package #:flime/swank-ext/common-lisp)

#-sbcl
(defun try-compile-file (file-path)
  (declare (ignore file-path))
  (error "Not supported implementation."))
