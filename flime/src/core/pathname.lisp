(defpackage #:flime/core/pathname
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:file #:coalton-library/file))
  (:export #:DirectoryPathname
           #:user-home?
           #:system-files-in-directory
           #:next-parent-directory
           #:find-parent-directory))
(in-package #:flime/core/pathname)

(named-readtables:in-readtable coalton:coalton)

(cl:deftype directory-pathname () '(cl:satisfies uiop:directory-pathname-p))

(coalton-toplevel
  (repr :native directory-pathname)
  (define-type DirectoryPathname)

  (define-instance (Eq DirectoryPathname)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:equal a b))))

  (define-instance (Into DirectoryPathname String)
    (define (into dir)
      (lisp String (dir)
        (cl:namestring dir))))

  (define-instance (Into String DirectoryPathname)
    (define (into s)
      (let ((path (as file:Pathname s)))
        (lisp DirectoryPathname (path)
          (cl:assert (uiop:directory-pathname-p path))
          path))))

  (define-instance (Into file:Pathname DirectoryPathname)
    (define (into path)
      (lisp DirectoryPathname (path)
        (uiop:pathname-directory-pathname path))))

  (define-instance (Into DirectoryPathname file:Pathname)
    (define (into path)
      (lisp file:Pathname (path)
        path)))

  (define-instance (Hash DirectoryPathname)
    (define (hash dir)
      (lisp Hash (dir)
        (cl:sxhash dir))))

  (declare user-home? (DirectoryPathname -> Boolean))
  (define (user-home? dir)
    (lisp Boolean (dir)
      (cl:equal (cl:probe-file dir)
                (cl:user-homedir-pathname))))

  (declare system-files-in-directory (DirectoryPathname -> (List file:Pathname)))
  (define (system-files-in-directory dir)
    (lisp (List file:Pathname) (dir)
      (uiop:directory-files dir "*.asd")))

  (declare next-parent-directory (DirectoryPathname -> (Optional DirectoryPathname)))
  (define (next-parent-directory dir)
    (if (user-home? dir)
        None
        (let ((parent (lisp DirectoryPathname (dir)
                        (uiop:pathname-parent-directory-pathname dir))))
          (if (== parent dir)
              None
              (Some parent)))))

  (declare find-parent-directory ((DirectoryPathname -> Boolean) -> DirectoryPathname -> (Optional DirectoryPathname)))
  (define (find-parent-directory pred dir)
    (if (pred dir)
        (Some dir)
        (match (next-parent-directory dir)
          ((Some parent)
           (find-parent-directory pred parent))
          ((None) None)))))
