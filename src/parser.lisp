(defpackage #:almighty-html/parser
  (:use #:cl)
  (:import-from #:almighty-html/dsl
                #:</>)
  (:import-from #:almighty-html/element
                #:render-to-string)
  (:local-nicknames (#:p #:parcom)
                    (#:x #:parcom/xml)))
(in-package #:almighty-html/parser)
;; (parcom/xml:xml (parcom:in (with-open-file (s "src/test.html"
;;                                              :direction :input
;;                                              :element-type 'character
;;                                              :external-format :default)
;;                              (let* ((len (file-length s))
;;                                     (buffer (make-string len)))
;;                                (read-sequence buffer s)
;;                                buffer))))

(defparameter *simple-element* (render-to-string (</> (p "This is the contents of the simple element."))))
(defparameter *complex-element* (render-to-string (</> (div :class "the-divs-class" :active "true"
                                                         (p :class "the-ps-class" "The P's contents.")))))

(defun parse-html (html-string)
  (x:xml (p:in html-string)))

#+nil
(parse-html *complex-element*)

(defun make-html-element (element) element)
#+nil
(make-html-element (parse-html *complex-element*))

(defun make-html-elements (document)
  (%make-html-elements (x:document-element document)))
(defun %make-html-elements (element)
  (typecase element
    (x:element
     (let ((content (x:element-content element)))
       (with-open-stream (s *standard-output*)
         (append (list (intern (string-upcase (x:element-name element))))
                 (loop :for (k v) :on (alexandria:hash-table-plist (x:element-metadata element)) :by #'cddr
                       :append (list (intern (string-upcase k) :keyword) v))
                 (list (typecase content
                         (string content)
                         (hash-table (%make-html-elements content))
                         (otherwise content)))))))
    ;; (cons (append (list (intern (string-upcase (first document))))
    ;;               (loop :for (k v) :on (alexandria:hash-table-plist (rest document)) :by #'cddr
    ;;                     :append (list (intern (string-upcase k) :keyword) v))))
    
    (hash-table (loop :for (k v) :on (alexandria:hash-table-plist element) :by #'cddr
                      :append (%make-html-elements v)))
    (otherwise (type-of element))))
#+nil
(make-html-elements (parse-html *complex-element*))
