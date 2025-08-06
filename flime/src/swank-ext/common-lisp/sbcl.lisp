(defpackage #:flime/swank-ext/common-lisp/sbcl
  (:use #:cl)
  (:import-from #:swank/source-path-parser)
  (:export #:try-compile-file))
(in-package #:flime/swank-ext/common-lisp/sbcl)

;;
;; `get-location' and related functions are from SBLint (https://github.com/cxxxr/sblint)
;; which is licensed under BSD 2-Clause.

(deftype ignorable-compiler-warning ()
  '(or #+asdf3.3 asdf/operate:recursive-operate
    ;; XXX: Actual redefinition should be warned, however it loads the same file twice when compile-time & load-time and it shows many redefinition warnings.
    sb-kernel:redefinition-warning
    uiop:compile-warned-warning))

(defun ignorable-compiler-warning-p (condition)
  (typep condition 'ignorable-compiler-warning))

(defun compiler-source-path (context)
  "Return the source-path for the current compiler error.
Returns NIL if this cannot be determined by examining internal
compiler state."
  (cond ((sb-c::node-p context)
         (reverse
          (sb-c::source-path-original-source
           (sb-c::node-source-path context))))
        ((sb-c::compiler-error-context-p context)
         (reverse
          (sb-c::compiler-error-context-original-source-path context)))))

(defun compiler-note-position (file context)
  (let* ((source-path (compiler-source-path context))
         (position (swank/source-path-parser:source-path-file-position source-path file)))
    (when position
      (1+ position))))

(defun file-position-to-line-and-column (file position)
  (let ((line 1)
        (column 0))
    (with-open-file (in file :direction :input :element-type 'character)
      (dotimes (i (1- position))
        (let ((char (read-char in)))
          (cond
            ((char= char #\Newline)
             (incf line)
             (setf column 0))
            ((char= char #\Return))
            (t (incf column))))))
    (values line column)))

(defun get-location (condition)
  (let* ((context (sb-c::find-error-context nil))
         (file (or (and context
                        (sb-c::compiler-error-context-file-name context))
                   *load-truename*
                   *compile-file-truename*))
         (position (cond
                     ((and (typep file '(or string pathname))
                           context)
                      (compiler-note-position file context))
                     ((typep condition 'reader-error)
                      (let ((stream (stream-error-stream condition)))
                        (file-position stream)))
                     ((and (typep condition 'sb-c::encapsulated-condition)
                           (typep (sb-int:encapsulated-condition condition)
                                  'sb-c::input-error-in-compile-file))
                      ;; reader-error in compile-file
                      (let ((stream (slot-value (sb-int:encapsulated-condition
                                                 (sb-int:encapsulated-condition condition))
                                                'stream)))
                        (setq file
                              (SB-IMPL::FD-STREAM-FILE
                               (slot-value (slot-value (sb-int:encapsulated-condition condition) 'condition)
                                           'sb-int::stream)))
                        (file-position stream)))
                     (t nil))))
    (multiple-value-bind (line column)
        (file-position-to-line-and-column file position)
      (values line column file))))

(defun try-compile-file (file-path)
  (let ((notes '()))
    (flet ((handle-condition (condition)
             (unless (ignorable-compiler-warning-p condition)
               (let ((sb-int:*print-condition-references* nil))
                 (multiple-value-bind (line column file)
                     (get-location condition)
                   (declare (ignore file))
                   (push
                    (list (1- line)
                          column
                          ;; FIXME: End position should be the end of the next element.
                          (1- line)
                          (1+ column)
                          (string-downcase (class-name (class-of condition)))
                          (princ-to-string condition))
                    notes))))))
      (handler-bind ((sb-c:fatal-compiler-error #'handle-condition)
                     (sb-c:compiler-error #'handle-condition)
                     (error #'handle-condition)
                     (warning #'handle-condition))
        (compile-file file-path))
      notes)))
