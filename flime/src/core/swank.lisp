(defpackage #:flime/core/swank
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:list #:coalton-library/list)
   (#:file #:coalton-library/file))
  (:export #:SwankProcess
           #:start-process
           #:stop-process
           #:eval-string-in-subprocess
           #:load-swank-extension
           #:completions
           #:documentation-symbol
           #:definitions-of-symbol))
(in-package #:flime/core/swank)

(named-readtables:in-readtable coalton:coalton)

(cl:defvar *flime-asd-file*
  (asdf:system-source-file :flime))

(coalton-toplevel
  (repr :native flime/swank/server:swank-server)
  (define-type SwankServer)

  (repr :native flime/swank/connection:connection)
  (define-type SwankConnection)

  (define-struct SwankProcess
    (server SwankServer)
    (connection SwankConnection)
    (extensions (Cell (List String))))

  (declare start-process (Unit -> SwankProcess))
  (define (start-process)
    (let ((server (lisp SwankServer ()
                    (flime/swank:create-swank-server)))
          (conn
            (lisp SwankConnection (server)
              (cl:let ((conn (flime/swank:connect-to-swank-server server)))
                (flime/swank:start-processing conn)
                conn))))
      ;; Load a swank extension for compiling Coalton file incrementally.
      (let ((process (SwankProcess server conn (cell:new (make-list)))))
        (eval-string-in-subprocess* process "(require 'asdf)")
        (eval-string-in-subprocess*
         process
         (lisp String ()
           ;; TODO: Error handling
           (cl:format cl:nil "(asdf:load-asd ~S)"
                      (uiop:native-namestring *flime-asd-file*))))
        process)))

  (declare stop-process (SwankProcess -> Unit))
  (define (stop-process swank)
    (let ((conn (.connection swank)))
      (lisp Unit (conn)
        (flime/swank:swank-rex-async
         '(swank:quit-lisp)
         conn)
        Unit)))

  (declare eval-string-in-subprocess (SwankProcess -> String -> String))
  (define (eval-string-in-subprocess swank code)
    (let ((conn (.connection swank)))
      (lisp String (conn code)
        (cl:first
         (flime/swank:swank-rex
          `(swank::eval-region (cl:format cl:nil "(prin1-to-string ~A)" ,code))
          conn)))))

  (declare eval-string-in-subprocess* (SwankProcess -> String -> Unit))
  (define (eval-string-in-subprocess* swank code)
    (let ((conn (.connection swank)))
      (lisp Unit (conn code)
            (flime/swank:swank-rex-async
             `(swank::eval-region (cl:format cl:nil "(prin1-to-string ~A)" ,code))
             conn)
            Unit)))

  (declare load-swank-extension (SwankProcess -> String -> Unit))
  (define (load-swank-extension process language-id)
    (unless (list:member language-id (cell:read (.extensions process)))
      (eval-string-in-subprocess
       process
       (lisp String (language-id)
         ;; TODO: Error handling
         (cl:format cl:nil "(asdf:load-system :flime/swank-ext/~(~A~))"
                    language-id)))
      (cell:update! (list:cons language-id)
                    (.extensions process))
      Unit))

  ;;
  ;; Completion

  (repr :enum)
  (define-type SymbolKind
    KindStruct
    KindClass
    KindConstructor
    KindFunction
    KindVariable
    KindKeyword
    KindUnknown)

  (define-instance (Into SymbolKind String)
    (define (into k)
      (match k
        ((KindStruct) "struct")
        ((KindClass) "class")
        ((KindConstructor) "constructor")
        ((KindFunction) "function")
        ((KindVariable) "variable")
        ((KindKeyword) "keyword")
        ((KindUnknown) "text"))))

  (define-struct CompletionSymbol
    (name String)
    (kind SymbolKind))

  (declare completions (SwankProcess -> String -> String -> String -> (List CompletionSymbol)))
  (define (completions swank prefix language-id package-name)
    (let ((conn (.connection swank)))
      (lisp (List CompletionSymbol) (conn prefix language-id package-name)
        (cl:mapcar (cl:lambda (item)
                     (cl:destructuring-bind (name cl:&rest metadata)
                         item
                       (cl:let ((name (cl:concatenate 'cl:string
                                                      prefix
                                                      (cl:subseq name (cl:length prefix))))
                                (kind (cl:or (cl:cdr (cl:assoc "kind" metadata :test 'cl:equal))
                                             "unknown")))
                         (coalton (CompletionSymbol
                                   (lisp String () name)
                                   (match (lisp String () kind)
                                     ("struct" KindStruct)
                                     ("class" KindClass)
                                     ("constructor" KindConstructor)
                                     ("function" KindFunction)
                                     ("variable" KindVariable)
                                     ("keyword" KindKeyword)
                                     ("unknown" KindUnknown)))))))
                   (cl:if (cl:equal language-id "coalton")
                          (flime/swank:swank-rex
                           `(flime/swank-ext/coalton:completions ,prefix ,package-name)
                           conn)
                          (cl:mapcar #'cl:list
                                     (cl:first
                                      (flime/swank:swank-rex
                                       `(swank:simple-completions ,prefix ,package-name)
                                       conn))))))))

  (declare documentation-symbol (SwankProcess -> String -> String -> String -> String))
  (define (documentation-symbol swank symbol-name _language-id package-name)
    (let ((conn (.connection swank)))
      (lisp String (conn symbol-name package-name)
        (flime/swank:swank-rex
         `(swank:documentation-symbol ,symbol-name)
         conn
         :package package-name))))

  (declare definitions-of-symbol (SwankProcess -> String -> String -> (List (Tuple file:Pathname UFix))))
  (define (definitions-of-symbol swank symbol-name package-name)
    (let ((conn (.connection swank)))
      (lisp (List (Tuple file:Pathname UFix)) (conn symbol-name package-name)
        (cl:mapcar (cl:lambda (def)
                     (cl:let ((location (cl:cdr (cl:assoc :location (cl:cdr def)))))
                       (cl:cons (cl:pathname (cl:second (cl:assoc :file location)))
                                (cl:second (cl:assoc :position location)))))
                   (flime/swank:swank-rex
                    `(swank:find-definitions-for-emacs ,symbol-name)
                    conn
                    :package package-name))))))
