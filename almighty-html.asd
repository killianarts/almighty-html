(defsystem "almighty-html"
  :version "1.3.0"
  :description "Component-oriented HTML DSL"
  :author "Micah Killian, Akira Tempaku, Bo Yao"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :in-order-to ((test-op (test-op almighty-html-test)))
  :pathname "src"
  :serial t
  :depends-on ("alexandria" "str" "cl-minify-css" "parcom" "parcom/xml")
  :components ((:file "utils")
               (:file "element")
               (:file "dsl")
               (:file "builtin")
               (:file "web-components")
               (:file "parser")
               (:file "main")))
