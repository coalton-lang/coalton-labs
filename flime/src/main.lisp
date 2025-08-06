(defpackage #:flime
  (:use #:cl)
  (:import-from #:coalton
                #:coalton
                #:lisp
                #:UFix)
  (:shadowing-import-from #:coalton-prelude
                          #:Optional
                          #:Some
                          #:None)
  (:local-nicknames
   (#:core #:flime/core)
   (#:workspace #:flime/core/workspace)
   (#:project #:flime/core/project)
   (#:text-doc #:flime/core/text-document)
   (#:pathname #:flime/core/pathname)
   (#:file #:coalton-library/file)
   (#:result #:coalton-library/result))
  (:export #:make-workspace
           #:initialize
           #:shutdown
           #:open-file
           #:get-file
           #:edit-file
           #:check-file
           #:compiler-note-position-start
           #:compiler-note-position-end
           #:compiler-note-file-path
           #:compiler-note-condition
           #:compiler-note-message
           #:close-file
           #:complete-symbol-at
           #:documentation-at
           #:definitions-at
           #:indent-file-line-at
           #:format-document))
(in-package #:flime)

(defun ensure-right-directory (path)
  (or (uiop:directory-exists-p
       (etypecase path
         (string
          (uiop:ensure-directory-pathname path))
         (pathname
          path)))
      (error "Directory not exists: ~A" path)))

(defun make-workspace (&optional root-path)
  (check-type root-path (or null string pathname))
  (let ((workspace (coalton (core:make-workspace))))
    (when root-path
      (let ((root-path (ensure-right-directory root-path)))
        (coalton (core:initialize (lisp workspace:Workspace () workspace)
                                  (lisp pathname:DirectoryPathname () root-path)))))
    workspace))

(defun initialize (workspace root-path)
  (let ((root-path (ensure-right-directory root-path)))
    (coalton (core:initialize (lisp workspace:Workspace () workspace)
                              (lisp pathname:DirectoryPathname () root-path)))
    (values)))

(defun shutdown (workspace)
  (coalton (core:shutdown (lisp workspace:Workspace () workspace)))
  (values))

(defun values-of-text-document (doc)
  (values
   (coalton
    (text-doc:text-document-path
     (lisp text-doc:TextDocument () doc)))
   (coalton
    (text-doc:text-document-version
     (lisp text-doc:TextDocument () doc)))))

(defmacro unwrap-or-nil (type &body coalton-body)
  `(catch 'end
     (coalton:coalton
      (coalton-prelude:unwrap-or-else
       (coalton:fn (x) x)
       (coalton:fn () (coalton:lisp ,type () (throw 'end coalton:Nil)))
       (coalton:progn ,@coalton-body)))))

(defun open-file (workspace file-path &key language-id (version 0) content)
  (check-type file-path (or string pathname))
  (check-type version (integer 0))
  (let* ((content (or content
                      (if (uiop:file-exists-p file-path)
                          (uiop:read-file-string file-path)
                          "")))
         (doc
           (coalton
            (core:open-file (lisp workspace:Workspace () workspace)
                            (core:make-text-document (lisp file:Pathname () file-path)
                                                     (lisp (Optional coalton:String) ()
                                                       (if (null language-id)
                                                           None
                                                           (Some language-id)))
                                                     (lisp UFix () version)
                                                     (lisp coalton:String () content))))))
    (values-of-text-document doc)))

(defun get-document (workspace file-path)
  (unwrap-or-nil text-doc:TextDocument
    (core:get-file (lisp workspace:Workspace () workspace)
                   (lisp file:Pathname () (cl:pathname file-path)))))

(defun get-file (workspace file-path)
  (check-type file-path (or string pathname))
  (let ((doc (get-document workspace file-path)))
    (when doc
      (values-of-text-document doc))))

(defun file-point-p (x)
  (and (consp x)
       (typep (car x) '(integer 0))
       (typep (cdr x) '(integer 0))))

(deftype file-point () '(satisfies file-point-p))

(defun coalton-point-to-file-point (point)
  (cons
   (coalton
    (text-doc:point-line (lisp text-doc:Point () point)))
   (coalton
    (text-doc:point-column (lisp text-doc:Point () point)))))

(defun file-point-to-coalton-point (file-point)
  (coalton
   (core:make-point (lisp UFix () (car file-point))
                    (lisp UFix () (cdr file-point)))))

(defun edit-file (workspace file-path text &key version start end)
  (check-type file-path (or string pathname))
  (check-type version (or null (integer 0)))
  (check-type text string)
  (check-type start (or null file-point))
  (check-type end (or null file-point))
  (assert (or (and start end)
              (and (null start) (null end))))

  (let ((change
          (if start
              (coalton
               (core:make-incremental-change (lisp coalton:String () text)
                                             (lisp text-doc:Point ()
                                               (file-point-to-coalton-point start))
                                             (lisp text-doc:Point ()
                                               (file-point-to-coalton-point end))))
              (coalton
               (core:make-full-change (lisp coalton:String () text))))))
    (values-of-text-document
     (if version
         (coalton
          (result:ok-or-error
           (core:edit-file (lisp workspace:Workspace () workspace)
                           (lisp file:Pathname () (pathname file-path))
                           (lisp text-doc:ContentChange () change)
                           (lisp UFix () version))))
         (coalton
          (result:ok-or-error
           (core:edit-file-without-version (lisp workspace:Workspace () workspace)
                                           (lisp file:Pathname () (pathname file-path))
                                           (lisp text-doc:ContentChange () change))))))))

(defun compiler-note-position-start (note)
  (coalton-point-to-file-point
   (coalton
    (project:compiler-note-position-start
     (lisp project:CompilerNote () note)))))

(defun compiler-note-position-end (note)
  (coalton-point-to-file-point
   (coalton
    (project:compiler-note-position-end
     (lisp project:CompilerNote () note)))))

(defun compiler-note-file-path (note)
  (coalton
   (project:compiler-note-file-path
    (lisp project:CompilerNote () note))))

(defun compiler-note-condition (note)
  (coalton
   (project:compiler-note-condition
    (lisp project:CompilerNote () note))))

(defun compiler-note-message (note)
  (coalton
   (project:compiler-note-message
    (lisp project:CompilerNote () note))))

(defun check-file (workspace file-path)
  (check-type file-path (or string pathname))
  (coalton
   (result:ok-or-error
    (core:check-file (lisp workspace:Workspace () workspace)
                     (lisp file:Pathname () (cl:pathname file-path))))))

(defun close-file (workspace file-path)
  (check-type file-path (or string pathname))
  (coalton
   (core:close-file (lisp workspace:Workspace () workspace)
                    (lisp file:Pathname () (cl:pathname file-path)))))

(defun find-project-of-file (workspace file-path)
  (check-type file-path (or string pathname))
  (unwrap-or-nil project:Project
    (workspace:find-project-of-file (lisp workspace:Workspace () workspace)
                                    (lisp file:Pathname () (cl:pathname file-path)))))

(defun text-edit-to-alist (edit)
  `(("range" . ,(cons
                 (coalton-point-to-file-point
                  (coalton
                   (text-doc:range-start (text-doc:text-edit-range (lisp text-doc:TextEdit () edit)))))
                 (coalton-point-to-file-point
                  (coalton
                   (text-doc:range-end (text-doc:text-edit-range (lisp text-doc:TextEdit () edit)))))))
    ("newText" . ,(coalton
                   (text-doc:text-edit-new-text (lisp text-doc:TextEdit () edit))))))

(defun completion-item-to-alist (completion-item)
  `(("label" . ,(project:completion-item-label completion-item))
    ("kind" . ,(project:completion-item-kind completion-item))
    ("textEdit" . ,(text-edit-to-alist (project:completion-item-text-edit completion-item)))))

(defun complete-symbol-at (workspace file-path point)
  (check-type file-path (or string pathname))
  (check-type point file-point)
  (let ((project (find-project-of-file workspace file-path)))
    (when project
      (mapcar
       #'completion-item-to-alist
       (coalton
        (core:complete-symbol-at (lisp project:Project () project)
                                 (lisp file:Pathname () (cl:pathname file-path))
                                 (lisp text-doc:Point () (file-point-to-coalton-point point))))))))

(defun documentation-at (workspace file-path point)
  (check-type file-path (or string pathname))
  (check-type point file-point)
  (let ((project (find-project-of-file workspace file-path)))
    (when project
      (unwrap-or-nil coalton:String
        (core:documentation-symbol-at
         (lisp project:Project () project)
         (lisp file:Pathname () (cl:pathname file-path))
         (lisp text-doc:Point () (file-point-to-coalton-point point)))))))

(defun definitions-at (workspace file-path point)
  (check-type file-path (or string pathname))
  (check-type point file-point)
  (let ((project (find-project-of-file workspace file-path)))
    (when project
      (mapcar (lambda (def)
                (destructuring-bind (file-path . pos) def
                  (cons file-path
                        ;; Find the line from the file position.
                        (with-open-file (in file-path)
                          (loop repeat pos
                                for ch = (read-char in nil nil)
                                while ch
                                summing (if (char= #\Newline ch)
                                            1
                                            0))))))
              (unwrap-or-nil (coalton:List (coalton-prelude:Tuple file:Pathname coalton:UFix))
                (core:definitions-of-symbol-at
                  (lisp project:Project () project)
                  (lisp file:Pathname () (cl:pathname file-path))
                  (lisp text-doc:Point () (file-point-to-coalton-point point))))))))

(defun indent-file-line-at (workspace file-path point)
  (check-type file-path (or string pathname))
  (check-type point file-point)
  (let ((doc (get-document workspace file-path)))
    (when doc
      (mapcar #'text-edit-to-alist
              (coalton
               (core:indent-line-at (lisp text-doc:TextDocument () doc)
                                    (lisp text-doc:Point () (file-point-to-coalton-point point))))))))

(defun format-document (workspace file-path)
  (check-type file-path (or string pathname))
  (let ((doc (get-document workspace file-path)))
    (when doc
      (mapcar #'text-edit-to-alist
              (coalton
               (core:format-text-document (lisp text-doc:TextDocument () doc)))))))
