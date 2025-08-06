(uiop:define-package #:flime/core
  (:mix #:cl
        #:flime/core/text-document
        #:flime/core/project
        #:flime/core/workspace)
  (:export #:make-workspace
           #:initialize
           #:shutdown
           #:make-text-document
           #:make-incremental-change
           #:make-full-change
           #:make-point
           #:open-file
           #:get-file
           #:edit-file
           #:edit-file-without-version
           #:check-file
           #:close-file
           #:complete-symbol-at
           #:indent-line-at
           #:documentation-symbol-at
           #:definitions-of-symbol-at
           #:format-text-document))
(in-package #:flime/core)
