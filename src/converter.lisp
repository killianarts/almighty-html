(defpackage #:almighty-html/converter
  (:use #:cl)
  (:export #:convert-html-to-almighty))

(in-package #:almighty-html/converter)

(defun read-file-to-string (path)
  "Read the contents of a file into a string."
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(defun convert-html-to-almighty (source output-path)
  "Convert HTML to almighty-html s-expression format and write to OUTPUT-PATH.
SOURCE is either a pathname to an HTML file or an almighty-html/parser:document-node.
Returns the output pathname."
  (let ((doc (etypecase source
               ((or string pathname)
                (almighty-html/parser:parse-html (read-file-to-string source)))
               (almighty-html/parser:document-node source))))
    (almighty-html/parser/serializer:write-lispified-node doc output-path)))
