;;;; t/package.lisp — Test package definition

(defpackage #:almighty-html/parser/tests
  (:use #:cl #:almighty-html/parser #:lisp-unit2)
  (:local-nicknames (#:dom #:almighty-html/parser/dom))
  (:export #:run-all-tests))

(in-package #:almighty-html/parser/tests)

(defun run-all-tests ()
  "Run the full test suite and report results."
  (let ((results (run-tests :package :almighty-html/parser/tests :run-contexts (list 'with-failure-debugging-context))))
    (print-summary results)))
