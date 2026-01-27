(defpackage #:almighty-html/utils
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-hash-table
                #:make-keyword
                #:symbolicate)
  (:export #:escape-html-attribute
           #:escape-html-text-content
           #:clsx))
(in-package #:almighty-html/utils)

(defparameter *text-content-escape-map*
  (alist-hash-table
   '((#\& . "&amp;")
     (#\< . "&lt;")
     (#\> . "&gt;")
     (#\" . "&quot;")
     (#\' . "&#x27;")
     (#\/ . "&#x2F;")
     (#\` . "&grave;")
     (#\= . "&#x3D;"))))

(defparameter *attribute-escape-map*
  (alist-hash-table
   '((#\" . "&quot;"))))

(defun escape-string (str escape-map)
  "Escape a string according to the map, returning a fresh string (or original if non-string)."
  (if (stringp str)
      (with-output-to-string (out)
        (loop for c across str
              for replacement = (gethash c escape-map)
              do (write-string (or replacement (string c)) out)))
      str))

(defun escape-html-text-content (str)
  (escape-string str *text-content-escape-map*))

(defun escape-html-attribute (str)
  (escape-string str *attribute-escape-map*))

(defun clsx (&rest strs)
  "Constructing class strings conditionally.
It removes `nil` from the string list, then joins the remaining strings with spaces."
  (format nil "~{~a~^ ~}" (remove nil strs)))
