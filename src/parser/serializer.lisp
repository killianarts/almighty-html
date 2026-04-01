 ;;;; serializer.lisp — DOM-to-HTML serializer
;;;; Converts a DOM tree back into an HTML string for testing and output.

(defpackage #:almighty-html/parser/serializer
  (:use #:cl)
  (:local-nicknames (#:dom #:almighty-html/parser/dom))
  (:export #:serialize-node
           #:lispify-node
           #:write-lispified-node))

(in-package #:almighty-html/parser/serializer)

(defun serialize-node (node &key (indent nil) (level 0))
  "Serialize a DOM node to an HTML string.
If INDENT is non-NIL, pretty-print with that many spaces per level."
  (with-output-to-string (s)
    (serialize-to-stream node s indent level)))

(defun serialize-to-stream (node stream indent level)
  (typecase node
    (dom:document-node
     (loop for child across (dom:node-children node)
           do (serialize-to-stream child stream indent level)))

    (dom:doctype-node
     (when indent (write-indent stream indent level))
     (format stream "<!DOCTYPE ~a>" (or (dom:doctype-name node) "html"))
     (when indent (write-char #\Newline stream)))

    (dom:text-node
     (let ((data (dom:text-data node)))
       (if indent
           (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) data)))
             (when (> (length trimmed) 0)
               (write-indent stream indent level)
               (write-string (escape-html-text trimmed) stream)
               (write-char #\Newline stream)))
           (write-string (escape-html-text data) stream))))

    (dom:comment-node
     (when indent (write-indent stream indent level))
     (format stream "<!--~a-->" (dom:comment-data node))
     (when indent (write-char #\Newline stream)))

    (dom:element-node
     (let ((tag (dom:element-tag-name node)))
       (when indent (write-indent stream indent level))
       (write-char #\< stream)
       (write-string tag stream)
       ;; Attributes
       (loop for (name . value) in (dom:element-attributes node)
             do (format stream " ~a=\"~a\"" name (escape-html-attribute value)))
       ;; Self-closing or void
       (cond
         ((member tag dom:*void-elements* :test #'string-equal)
          (write-string ">" stream)
          (when indent (write-char #\Newline stream)))
         (t
          (write-string ">" stream)
          (when indent (write-char #\Newline stream))
          ;; Children — raw text elements get unescaped output
          (if (member tag dom:*raw-text-elements* :test #'string-equal)
              (loop :for child :across (dom:node-children node)
                    :do (when (typep child 'dom:text-node)
                          (write-string (dom:text-data child) stream)))
              (loop :for child :across (dom:node-children node)
                    :do (serialize-to-stream child stream indent (1+ level))))
          ;; Closing tag
          (when indent (write-indent stream indent level))
          (format stream "</~a>" tag)
          (when indent (write-char #\Newline stream))))))))

(defun lispify-node (node &key (indent 2) (level 0))
  "Serialize a DOM node to a Lisp s-expression string.
INDENT controls indentation (default 2 spaces per level, NIL for compact output)."
  (with-output-to-string (s)
    (serialize-to-lisp node s indent level)))

(defun escape-lisp-string (string)
  "Escape a string for inclusion in a Lisp double-quoted string literal."
  (with-output-to-string (s)
    (loop for ch across string
          do (case ch
               (#\\ (write-string "\\\\" s))
               (#\" (write-string "\\\"" s))
               (t   (write-char ch s))))))

(defun lisp-visible-p (node indent)
  "Return T if NODE would produce output in lispified form."
  (typecase node
    (dom:doctype-node nil)
    (dom:comment-node nil)
    (dom:text-node
     (if indent
         (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) (dom:text-data node))))
           (> (length trimmed) 0))
         t))
    (t t)))

(defun serialize-to-lisp (node stream indent level)
  "Serialize NODE to stream. Each call writes its content without a trailing newline."
  (typecase node
    (dom:document-node
     (let ((visible (remove-if-not (lambda (c) (lisp-visible-p c indent))
                                   (coerce (dom:node-children node) 'list))))
       (loop for (child . rest) on visible
             do (serialize-to-lisp child stream indent level)
                (when (and indent rest)
                  (write-char #\Newline stream)))))

    (dom:doctype-node nil)

    (dom:text-node
     (let ((data (dom:text-data node)))
       (if indent
           ;; Removed Space -- we want to keep spaces because we need space between <i>/<b> elements within <p> elements.
           (let ((trimmed (string-trim '(#\Tab #\Newline) data)))
             (when (> (length trimmed) 0)
               (write-indent stream indent level)
               (write-char #\" stream)
               (write-string (escape-lisp-string trimmed) stream)
               (write-char #\" stream)))
           (progn
             (write-char #\" stream)
             (write-string (escape-lisp-string data) stream)
             (write-char #\" stream)))))

    (dom:comment-node nil)

    (dom:element-node
     (write-char #\Newline stream)
     (let ((tag (dom:element-tag-name node)))
       (when indent (write-indent stream indent level))
       (write-char #\( stream)
       (write-string tag stream)
       ;; Attributes
       (loop :for (name . value) :in (dom:element-attributes node)
             :do (format stream " :~a \"~a\"" name (escape-lisp-string value)))
       ;; Self-closing or void
       (cond
         ((member tag dom:*void-elements* :test #'string-equal)
          (write-char #\) stream))
         (t
          (let ((visible (when indent
                           (remove-if-not (lambda (c) (lisp-visible-p c indent))
                                          (coerce (dom:node-children node) 'list)))))
            (cond
              ((and indent (null visible))
               (write-char #\) stream))
              ((null indent)
               (loop :for child :across (dom:node-children node)
                     :do (serialize-to-lisp child stream nil level))
               (write-char #\) stream))
              (t
               ;; This is causing the newline before text nodes
               ;; (write-char #\Newline stream)
               ;; (loop :for (child . rest) :on visible
               ;;       :do (serialize-to-lisp child stream indent (1+ level))
               ;;           (when rest (write-char #\Newline stream)))
               (loop :for (child . rest) :on visible
                     :do (serialize-to-lisp child stream 1 1))
               (write-char #\) stream))))))))))

(defun write-lispified-node (node-or-string path &key (indent 2) (if-exists :supersede))
  "Write the lispified representation of a DOM node to a file at PATH.
NODE-OR-STRING can be a DOM node (which will be lispified) or a string
already produced by LISPIFY-NODE.
INDENT controls indentation (default 2 spaces per level, NIL for no indentation).
IF-EXISTS is passed to OPEN (default :supersede)."
  (with-open-file (stream path :direction :output
                               :if-exists if-exists
                               :if-does-not-exist :create
                               :external-format :utf-8)
    (if (stringp node-or-string)
        (write-string node-or-string stream)
        (serialize-to-lisp node-or-string stream indent 0)))
  path)

(defun write-indent (stream indent level)
  (loop :repeat (* indent level) :do (write-char #\Space stream)))

(defun escape-html-text (text)
  "Escape text for safe inclusion in HTML content."
  (with-output-to-string (s)
    (loop for ch across text
          do (case ch
               (#\& (write-string "&amp;" s))
               (#\< (write-string "&lt;" s))
               (#\> (write-string "&gt;" s))
               (t   (write-char ch s))))))

(defun escape-html-attribute (value)
  "Escape text for safe inclusion in an HTML attribute value."
  (with-output-to-string (s)
    (loop for ch across value
          do (case ch
               (#\& (write-string "&amp;" s))
               (#\" (write-string "&quot;" s))
               (#\< (write-string "&lt;" s))
               (#\> (write-string "&gt;" s))
               (t   (write-char ch s))))))
