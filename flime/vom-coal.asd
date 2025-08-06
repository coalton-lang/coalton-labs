(defsystem "vom-coal"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "A wrapper library for Vom, a tiny logging library written in Common Lisp."
  :depends-on ("vom"
               "coalton")
  :components
  ((:file "src/vom-coal")))
