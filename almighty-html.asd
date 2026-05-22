(defsystem "almighty-html"
  :version "1.5.0"
  :description "Component-oriented HTML DSL with a built-in HTML parser"
  :author "Micah Killian, Akira Tempaku, Bo Yao"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("alexandria" "str")
  :components ((:module "parser"
                :serial t
                :components ((:file "dom")
                             (:file "input-stream")
                             (:file "serializer")
                             (:file "parser")
                             (:file "package")))
               (:file "utils")
               (:file "element")
               (:file "dsl")
               (:file "builtin")
               (:file "web-components")
               (:file "converter")
               (:file "main")))

(defsystem "almighty-html/tests"
  :description "Tests for almighty-html"
  :depends-on (#:almighty-html #:lisp-unit2)
  :serial t
  :components ((:module "tests/parser"
                :serial t
                :components ((:file "package")
                             (:file "dom-tests")
                             (:file "tree-builder-tests")
                             (:file "integration-tests")))
               (:module "tests/html"
                :serial t
                :depends-on ("tests/parser")
                :components ((:file "html-file-tests")))))
