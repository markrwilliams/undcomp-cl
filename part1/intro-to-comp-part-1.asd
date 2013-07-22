(defsystem #:intro-to-comp-part-1
  :description "Common Lisp port of Tom Stuart's \"Introduction to Computation\""
  :version "0.1"
  :author "markrwilliams@gmail.com"
  :license "Public Domain"
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "hash-funcs")
                         (:file "ast-nodes" :depends-on ("hash-funcs"))
                         (:file "machine" :depends-on ("ast-nodes"))))))
