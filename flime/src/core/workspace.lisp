(defpackage #:flime/core/workspace
  (:use #:coalton
        #:coalton-prelude)
  (:import-from #:flime/core/text-document
                #:TextDocument
                #:Point
                #:ContentChange
                #:text-document-path
                #:find-symbol-before
                #:TextEdit
                #:format-text-document)
  (:import-from #:flime/core/project
                #:Project
                #:ProjectError
                #:make-project
                #:finalize-project
                #:find-project-root
                #:project-root-directory?
                #:open-file-in-project
                #:get-file-in-project
                #:edit-file-in-project
                #:compile-file-in-project
                #:close-file-in-project)
  (:import-from #:flime/core/pathname
                #:DirectoryPathname)
  (:local-nicknames
   (#:ht #:coalton-library/hashtable)
   (#:file #:coalton-library/file)
   (#:iter #:coalton-library/iterator))
  (:export #:Workspace
           #:WorkspaceError
           #:make-workspace
           #:initialize
           #:shutdown
           #:find-project-of-file
           #:open-file
           #:get-file
           #:edit-file
           #:edit-file-without-version
           #:check-file
           #:close-file))
(in-package #:flime/core/workspace)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-struct Workspace
    (project-map (Hashtable DirectoryPathname Project)))

  (define (make-workspace)
    (Workspace (ht:new)))

  (declare find-project-of-file (Workspace -> file:Pathname -> (Optional Project)))
  (define (find-project-of-file workspace file-path)
    (match (find-project-root file-path)
      ((Some root-path)
       (ht:get (.project-map workspace) root-path))
      ((None) None)))

  (declare find-or-create-project-of-file! (Workspace -> file:Pathname -> Project))
  (define (find-or-create-project-of-file! workspace file-path)
    (let project-map = (.project-map workspace))
    (let ((find-or-create (fn (root-path)
                            (match (ht:get project-map (the DirectoryPathname root-path))
                              ((Some prj)
                               prj)
                              ((None)
                               (let ((new-project (make-project root-path)))
                                 (ht:set! project-map
                                          root-path
                                          new-project)
                                 new-project))))))
      (match (find-project-root file-path)
        ((Some root-path)
         (find-or-create root-path))
        ((None)
         (find-or-create
          (lisp DirectoryPathname (file-path)
            (uiop:pathname-parent-directory-pathname file-path)))))))

  (define-type WorkspaceError
    (CantOpen ProjectError)
    (CantEdit ProjectError)
    (NoProjectFound file:Pathname))

  (declare initialize ((Into :a DirectoryPathname) => Workspace -> :a -> (Result WorkspaceError Project)))
  (define (initialize workspace file-path)
    (let ((root-path (as DirectoryPathname file-path)))
      (if (project-root-directory? root-path)
          (let ((project-map (.project-map workspace)))
            (Ok
             (match (ht:get project-map root-path)
               ((Some prj)
                prj)
               ((None)
                (let ((new-project (make-project root-path)))
                  (ht:set! project-map
                           root-path
                           new-project)
                  new-project)))))
          (Err (error (NoProjectFound (into root-path)))))))

  (declare shutdown (Workspace -> Unit))
  (define (shutdown workspace)
    (let projects-iter = (ht:entries (.project-map workspace)))
    (while-let (Some (Tuple _dir project)) = (iter:next! projects-iter)
      (finalize-project project)))

  (define-instance (Signalable WorkspaceError)
    (define (error err)
      (match err
        ((NoProjectFound path)
         (error (lisp String (path)
                  (cl:format cl:nil "No project found for '~A'." (uiop:native-namestring path)))))
        ((CantOpen e)
         (error e))
        ((CantEdit e)
         (error e)))))

  (define (open-file workspace doc)
    (let prj = (find-or-create-project-of-file! workspace (text-document-path doc)))
    (match (open-file-in-project prj doc)
      ((Ok new-doc)
       new-doc)
      ((Err e)
       (error (CantOpen e)))))

  (define (get-file workspace file-path)
    (match (find-project-of-file workspace file-path)
      ((Some prj)
       (get-file-in-project prj file-path))
      ((None)
       None)))

  (declare %edit-file (Workspace -> file:Pathname -> ContentChange -> (Optional UFix) -> (Result WorkspaceError TextDocument)))
  (define (%edit-file workspace file-path change version)
    (match (find-project-of-file workspace file-path)
      ((Some prj)
       (match (edit-file-in-project prj file-path change version)
         ((Ok new-doc)
          (Ok new-doc))
         ((Err e)
          (error (CantEdit e)))))
      ((None)
       (error (NoProjectFound file-path)))))

  (define (edit-file-without-version workspace file-path change)
    (%edit-file workspace file-path change None))

  (define (edit-file workspace file-path change version)
    (%edit-file workspace file-path change (Some version)))

  (define (check-file workspace file-path)
    (match (find-project-of-file workspace file-path)
      ((Some prj)
       (compile-file-in-project prj file-path))
      ((None)
       (error (NoProjectFound file-path)))))

  (define (close-file workspace file-path)
    (match (find-project-of-file workspace file-path)
      ((Some prj)
       (close-file-in-project prj file-path))
      ((None)
       False))))
