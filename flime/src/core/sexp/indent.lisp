(defpackage #:flime/core/sexp/indent
  (:use #:cl)
  (:export #:*coalton-indent-rules*
           #:calc-indent-level))
(in-package #:flime/core/sexp/indent)

(defparameter *coalton-indent-rules*
  '((define (4 &body))
    (let ((&whole 4 &rest (&whole 1 1 2)) &body))
    (while-let (as let))
    (fn (2 &body))
    (match (4 &rest (&whole 2 &rest 1)))
    (cond (&rest (&whole 2 &rest nil)))
    (lisp (6 4 &body))
    (return 0)
    (when 1)
    (unless 1)
    (while 1)
    (progn 0)
    (do 0)
    (package (4 &rest (&whole 2 &rest nil)))))

(defun form-path-p (path)
  (and (listp path)
       (every (lambda (v) (typep v '(integer 0)))
              path)))

(deftype form-path ()
  '(satisfies form-path-p))

(defun extract-&whole-rule (rule)
  (if (eq (first rule) '&whole)
      (destructuring-bind (whole-key indent &rest rest-rules)
          rule
        (declare (ignore whole-key))
        (check-type indent integer)
        (values indent rest-rules))
      (values nil rule)))

(defun make-circular-list (elem)
  (let ((cycle (make-list 1 :initial-element elem)))
    (nconc cycle cycle)))

(defun ensure-list (v)
  (if (listp v)
      v
      (list v)))

(defun compile-cons-indent-rule (rule)
  (check-type rule cons)
  (assert (not (eq (first rule) 'as)))
  (flet ((recursive-compile (subrule)
           (multiple-value-bind (indent subrule)
               (extract-&whole-rule subrule)
             (nconc (ensure-list indent)
                    (compile-cons-indent-rule subrule)))))
    (loop for (x . xs) on rule
          append (etypecase x
                   (integer (list x))
                   (symbol
                    (return
                      (nconc result
                             (ecase x
                               (&rest
                                (assert (null (cdr xs)))
                                (let ((subrule (car xs)))
                                  (make-circular-list
                                   (etypecase subrule
                                     (integer subrule)
                                     (null nil)
                                     (cons
                                      (recursive-compile subrule))))))
                               (&body
                                (assert (null xs))
                                (make-circular-list 2))))))
                   (cons
                    (list (recursive-compile x))))
          into result
          finally (return result))))

(defun compile-indent-rule (rule)
  (etypecase rule
    (integer (lambda (path)
               (destructuring-bind (x &rest xs)
                   path
                 (check-type x integer)
                 (cond
                   (xs nil)
                   ((< x rule) 4)
                   (t 2)))))
    (cons
     (case (first rule)
       (as (error "Need to handle outside of this function"))
       (otherwise
        (let ((indent-rule-list
                (compile-cons-indent-rule rule)))
          (lambda (path)
            (block nil
              (let ((subrule
                      (reduce (lambda (acc x)
                                (etypecase acc
                                  (integer (return nil))
                                  (cons (nth (1+ x) acc))
                                  (null nil)))
                              path
                              :initial-value (cons 0 indent-rule-list))))
                (etypecase subrule
                  (cons (first subrule))
                  (integer subrule)
                  (null nil)))))))))))

(defun calc-indent-level (function-name path)
  (check-type function-name string)
  (check-type path form-path)
  (let ((indent-rule (second
                      (assoc function-name
                             *coalton-indent-rules*
                             :test 'string-equal))))
    (if indent-rule
        (if (and (consp indent-rule)
                 (eq (first indent-rule) 'as))
            (calc-indent-level (symbol-name (second indent-rule)) path)
            (funcall (compile-indent-rule indent-rule) path))
        nil)))
