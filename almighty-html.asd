(defsystem "almighty-html"
  :version "1.4.0"
  :description "Component-oriented HTML DSL"
  :author "Micah Killian, Akira Tempaku, Bo Yao"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("alexandria" "str" "cl-minify-css" "almighty-html-parser")
  :components ((:file "utils")
               (:file "element")
               (:file "dsl")
               (:file "builtin")
               (:file "web-components")
               (:file "converter")
               (:file "main")))
