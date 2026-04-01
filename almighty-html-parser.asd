(defsystem "almighty-html-parser"
  :description "A recursive descent HTML parser with no external dependencies."
  :version "0.2.0"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:module "parser"
                :serial t
                :components ((:file "dom")
                             (:file "input-stream")
                             (:file "serializer")
                             (:file "parser")
                             (:file "package")))))

(defsystem "almighty-html-parser/tests"
  :description "Tests for almighty-html/parser"
  :depends-on (#:almighty-html/parser #:lisp-unit2)
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
