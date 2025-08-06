(defpackage #:flime/core/project
  (:use #:coalton
        #:coalton-prelude
        #:flime/core/text-document)
  (:shadowing-import-from #:flime/core/text-document
                          #:Range
                          #:Point
                          #:write-text-document
                          #:range-start
                          #:range-end
                          #:find-symbol-before
                          #:text-document-package-name
                          #:text-document-language-id
                          #:make-point
                          #:make-range
                          #:TextEdit)
  (:import-from #:flime/core/pathname
                #:DirectoryPathname
                #:find-parent-directory
                #:system-files-in-directory)
  (:import-from #:flime/core/swank
                #:SwankProcess)
  (:local-nicknames
   (#:ht #:coalton-library/hashtable)
   (#:file #:coalton-library/file)
   (#:list #:coalton-library/list)
   (#:str #:coalton-library/string)
   (#:sw #:flime/core/swank))
  (:export #:Project
           #:make-project
           #:finalize-project
           #:find-project-root
           #:project-root-directory?
           #:open-file-in-project
           #:get-file-in-project
           #:edit-file-in-project
           #:compile-file-in-project
           #:close-file-in-project
           #:fine-project-root
           #:ProjectError
           #:CompilerNote
           #:compiler-note-position-start
           #:compiler-note-position-end
           #:compiler-note-file-path
           #:compiler-note-condition
           #:compiler-note-message
           #:complete-symbol-at
           #:CompletionItem
           #:completion-item-label
           #:completion-item-kind
           #:completion-item-text-edit
           #:documentation-symbol-at
           #:definitions-of-symbol-at))
(in-package #:flime/core/project)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-struct TextDocumentMap
    ;; XXX: Using file:Pathname as keys is better, but its Hash method doesn't exist in Coalton.
    (hash (Hashtable String TextDocument)))

  (define (make-text-document-map)
    (TextDocumentMap (ht:new))) ;; XXX: Thread-safe

  (declare open-text-document (TextDocumentMap -> TextDocument -> TextDocument))
  (define (open-text-document map new-doc)
    (let ((hash (.hash map))
          (path (text-document-path new-doc)))
      (match (ht:get hash (into path))
        ((Some doc)
         (if (< (text-document-version doc) (text-document-version new-doc))
             (progn
               (ht:set! hash (into path) new-doc)
               new-doc)
             doc))
        ((None)
         (ht:set! hash (into path) new-doc)
         new-doc))))

  (declare get-text-document (TextDocumentMap -> file:Pathname -> (Optional TextDocument)))
  (define (get-text-document map path)
    (ht:get (.hash map) (into path)))

  (declare close-text-document (TextDocumentMap -> file:Pathname -> Boolean))
  (define (close-text-document map path)
    (let ((hash (.hash map)))
      (match (ht:get hash (into path))
        ((Some _)
         (ht:remove! hash (into path))
         True)
        ((None)
         False))))

  (define-struct Project
    (root-path DirectoryPathname)
    (doc-map TextDocumentMap)
    (process SwankProcess))

  (define (make-project root-path)
    (let process = (sw:start-process))
    ;; Add the project root to the source registry.
    (sw:eval-string-in-subprocess
     process
     (lisp String (root-path)
       (cl:format cl:nil "(push ~S asdf:*central-registry*)" root-path)))
    ;; Load lsp-init.lisp.
    (let init-file = (file:merge root-path "lsp-init.lisp"))
    (match (file:file-exists? init-file)
      ((Ok exists?)
       (when exists?
         (sw:eval-string-in-subprocess
          process
          (lisp String (init-file)
            (cl:format cl:nil "(handler-case (handler-bind ((error #'uiop:print-condition-backtrace)) (load ~S)) (error () nil))" (uiop:native-namestring init-file))))
         Unit))
      (_ Unit))
    (Project root-path (make-text-document-map) process))

  (declare finalize-project (Project -> Unit))
  (define (finalize-project project)
    (sw:stop-process (.process project)))

  (define-type ProjectError
    (AlreadyOpen file:Pathname)
    (NoTextDocument file:Pathname)
    (UnsupportedLanguage String)
    (CompilationFailure file:Pathname))

  (define-instance (Signalable ProjectError)
    (define (error err)
      (match err
        ((AlreadyOpen file-path)
         (error (lisp String (file-path)
                  (cl:format cl:nil "File is already open: ~A" (uiop:native-namestring file-path)))))
        ((NoTextDocument file-path)
         (error (lisp String (file-path)
                  (cl:format cl:nil "No open text document: ~A" (uiop:native-namestring file-path)))))
        ((UnsupportedLanguage language-id)
         (error (lisp String (language-id)
                  (cl:format cl:nil "Unsupported language: ~A" language-id))))
        ((CompilationFailure file-path)
         (error (lisp String (file-path)
                  (cl:format cl:nil "Compilation failure: ~A" (uiop:native-namestring file-path))))))))

  (declare open-file-in-project (Project -> TextDocument -> (Result ProjectError TextDocument)))
  (define (open-file-in-project project new-doc)
    (match (get-text-document (.doc-map project) (text-document-path new-doc))
      ((Some _)
       (error (AlreadyOpen (text-document-path new-doc))))
      ((None)
       (match (text-document-language-id new-doc)
         ((Some language-id)
          (sw:load-swank-extension (.process project) language-id))
         (_ Unit))
       (Ok (open-text-document (.doc-map project) new-doc)))))

  (declare get-file-in-project (Project -> file:Pathname -> (Optional TextDocument)))
  (define (get-file-in-project project file-path)
    (get-text-document (.doc-map project) file-path))

  (declare edit-file-in-project (Project -> file:Pathname -> ContentChange -> (Optional Version) -> (Result ProjectError TextDocument)))
  (define (edit-file-in-project project file-path change optional-version)
    (match (get-text-document (.doc-map project) file-path)
      ((Some doc)
       (let ((new-version (match optional-version
                            ((Some v) v)
                            ((None) (1+ (text-document-version doc)))))
             (new-doc (apply-content-change doc change new-version)))
         (ht:set! (.hash (.doc-map project)) (into file-path) new-doc)
         (Ok new-doc)))
      ((None)
       (error (NoTextDocument file-path)))))

  (define-struct CompilerNote
    (position Range)
    (file-path file:Pathname)
    (condition String)
    (message String))

  (declare compiler-note-position-start (CompilerNote -> Point))
  (define (compiler-note-position-start note)
    (.start (.position note)))
  (declare compiler-note-position-end (CompilerNote -> Point))
  (define (compiler-note-position-end note)
    (.end (.position note)))
  (declare compiler-note-file-path (CompilerNote -> file:Pathname))
  (define (compiler-note-file-path note) (.file-path note))
  (declare compiler-note-condition (CompilerNote -> String))
  (define (compiler-note-condition note) (.condition note))
  (declare compiler-note-message (CompilerNote -> String))
  (define (compiler-note-message note) (.message note))

  (declare compile-file-in-project (Project -> file:Pathname -> (Result ProjectError (List CompilerNote))))
  (define (compile-file-in-project project file-path)
    (match (get-text-document (.doc-map project) file-path)
      ((Some doc)
       (match (text-document-language-id doc)
         ((Some language-id)
          (if (list:member language-id (make-list "coalton" "common-lisp"))
              (let ((swank-result-str
                      (file:with-temp-file (lisp String (file-path)
                                             (cl:format cl:nil ".~A"
                                                        (cl:pathname-type file-path)))
                        (fn (stream)
                          (write-text-document stream doc)
                          (file:flush stream)
                          (Ok
                           (sw:eval-string-in-subprocess
                            (.process project)
                            (lisp String (stream language-id)
                              (cl:format cl:nil "(flime/swank-ext/~A:try-compile-file ~S)"
                                         language-id
                                         (uiop:native-namestring (cl:pathname stream))))))))))
                (match swank-result-str
                  ((Ok result-str)
                   (Ok
                    (lisp (List CompilerNote) (result-str file-path)
                      (cl:mapcar (cl:lambda (err)
                                   (cl:destructuring-bind (s-line s-col e-line e-col note message)
                                       err
                                     (CompilerNote
                                      (make-range (make-point s-line s-col)
                                                  (make-point e-line e-col))
                                      file-path
                                      note
                                      message)))
                                 (uiop:safe-read-from-string result-str)))))
                  ((Err _)
                   (Err (CompilationFailure file-path)))))
              (error (UnsupportedLanguage language-id))))
         (_ (Ok Nil))))
      ((None)
       (error (NoTextDocument file-path)))))

  (declare close-file-in-project (Project -> file:Pathname -> Boolean))
  (define (close-file-in-project project path)
    (close-text-document (.doc-map project) path))

  (define (project-root-directory? dir)
    (/= Nil (system-files-in-directory dir)))

  (define (find-project-root path)
    (find-parent-directory project-root-directory? (into path)))

  (define-struct CompletionItem
    (label String)
    (kind String)
    (text-edit TextEdit))

  (declare completion-item-label (CompletionItem -> String))
  (define completion-item-label .label)
  (declare completion-item-kind (CompletionItem -> String))
  (define completion-item-kind .kind)
  (declare completion-item-text-edit (CompletionItem -> TextEdit))
  (define completion-item-text-edit .text-edit)

  (declare complete-symbol-at (Project -> file:Pathname -> Point -> (List CompletionItem)))
  (define (complete-symbol-at project file-path point)
    (match (get-file-in-project project file-path)
      ((Some doc)
       (match (find-symbol-before doc point)
         ((Ok symbol)
          (let ((range (make-range (make-point (.line point) (- (.column point)
                                                                (str:length symbol)))
                                   point)))
            (map (fn (item)
                   (CompletionItem
                    (.name item)
                    (as String (.kind item))
                    (TextEdit range (.name item))))
                 (sw:completions (.process project)
                                 symbol
                                 (with-default "coalton" (text-document-language-id doc))
                                 (text-document-package-name doc)))))
         ((Err _) Nil)))
      ((None)
       Nil)))

  (declare documentation-symbol-at (Project -> file:Pathname -> Point -> (Optional String)))
  (define (documentation-symbol-at project file-path point)
    (match (get-file-in-project project file-path)
      ((Some doc)
       (match (find-symbol-on doc point)
         ((Ok (Some symbol-name))
          (Some
           (sw:documentation-symbol (.process project)
                                    symbol-name
                                    (with-default "coalton" (text-document-language-id doc))
                                    (text-document-package-name doc))))
         (_ None)))
      ((None) None)))

  (declare definitions-of-symbol-at (Project -> file:Pathname -> Point -> (Optional (List (Tuple file:Pathname UFix)))))
  (define (definitions-of-symbol-at project file-path point)
    (match (get-file-in-project project file-path)
      ((Some doc)
       (match (find-symbol-on doc point)
         ((Ok (Some symbol-name))
          (Some
           (sw:definitions-of-symbol (.process project)
             symbol-name
             (text-document-package-name doc))))
         (_ None)))
      ((None) None))))
