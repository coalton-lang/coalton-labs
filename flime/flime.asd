(defsystem "flime"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("flime/core")
  :pathname "src"
  :components
  ((:file "main")))

(defsystem "flime/core"
  :depends-on ("flime/swank"
               "coalton"
               "coal-rope"
               "swank"
               "flime/swank-ext/coalton")
  :pathname "src"
  :components
  ((:file "core" :depends-on ("core-subpackages"))
   (:module "core-subpackages"
    :pathname "core"
    :serial t
    :components ((:file "pathname")
                 (:file "swank")
                 (:module "sexp"
                  :components ((:file "indent")))
                 (:file "text-document")
                 (:file "project")
                 (:file "workspace")))))

(defsystem "flime/swank"
  :depends-on ("swank"
               "bordeaux-threads"
               "usocket"
               "babel"
               "alexandria")
  :pathname "src/swank"
  :serial t
  :components
  ((:file "connection")
   (:file "shell")
   (:file "server")
   (:file "protocol")
   (:file "client")
   (:file "main")))

(defsystem "flime/swank-ext/coalton"
  :depends-on ("coalton"
               "swank")
  :components
  ((:file "src/swank-ext/coalton")))

(defsystem "flime/swank-ext/common-lisp"
  :depends-on ((:feature :sbcl "swank"))
  :serial t
  :components
  ((:file "src/swank-ext/common-lisp/sbcl" :if-feature :sbcl)
   (:file "src/swank-ext/common-lisp")))
