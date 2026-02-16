;;;; dom.lisp — DOM node types following the WHATWG DOM specification.
;;;; Provides the tree structure that the tree builder constructs.

(defpackage #:almighty-html/parser/dom
  (:use #:cl)
  (:export
   ;; Node base class
   #:node
   #:node-parent
   #:node-children
   #:node-type
   ;; Document node
   #:document-node
   #:document-doctype
   #:document-quirks-mode
   #:make-document
   #:document-children
   ;; Element node
   #:element-node
   #:element-tag-name
   #:element-namespace
   #:element-attributes
   #:element-attribute
   #:make-element
   ;; Text node
   #:text-node
   #:text-data
   #:make-text-node
   ;; Comment node
   #:comment-node
   #:comment-data
   #:make-comment-node
   ;; Doctype node
   #:doctype-node
   #:doctype-name
   #:doctype-public-id
   #:doctype-system-id
   #:make-doctype-node
   ;; Tree manipulation
   #:append-child
   #:insert-child-before
   #:remove-child
   #:last-child
   ;; Query utilities
   #:child-elements
   #:child-elements-by-tag
   #:first-child-element
   #:get-element-by-id
   #:get-elements-by-tag-name
   #:inner-text
   ;; Element classification
   #:*void-elements*
   #:*raw-text-elements*))

(in-package #:almighty-html/parser/dom)

;;; ============================================================
;;; Node base class
;;; ============================================================

(defclass node ()
  ((parent :initarg :parent :initform nil :accessor node-parent)
   (children :initarg :children :initform (make-array 0 :adjustable t :fill-pointer 0)
             :accessor node-children)))

(defgeneric node-type (node)
  (:documentation "Return a keyword identifying the node type."))

;;; ============================================================
;;; Document node — root of the DOM tree
;;; ============================================================

(defclass document-node (node)
  ((doctype :initarg :doctype :initform nil :accessor document-doctype)
   (quirks-mode :initarg :quirks-mode :initform :no-quirks :accessor document-quirks-mode)))

(defmethod node-type ((node document-node)) :document)

(defun make-document ()
  (make-instance 'document-node))

(defun document-children (doc)
  "Return the children vector of a document."
  (node-children doc))

;;; ============================================================
;;; Element node
;;; ============================================================

(defclass element-node (node)
  ((tag-name :initarg :tag-name :initform "" :accessor element-tag-name)
   (namespace :initarg :namespace :initform :html :accessor element-namespace)
   (attributes :initarg :attributes :initform nil :accessor element-attributes)))

(defmethod node-type ((node element-node)) :element)

(defun make-element (tag-name &key (namespace :html) attributes)
  (make-instance 'element-node
                 :tag-name tag-name
                 :namespace namespace
                 :attributes (copy-list attributes)))

(defun element-attribute (element name)
  "Get attribute value by name, or NIL."
  (cdr (assoc name (element-attributes element) :test #'string=)))

(defun (setf element-attribute) (value element name)
  "Set an attribute on an element."
  (let ((pair (assoc name (element-attributes element) :test #'string=)))
    (if pair
        (setf (cdr pair) value)
        (push (cons name value) (element-attributes element))))
  value)

;;; ============================================================
;;; Text node
;;; ============================================================

(defclass text-node (node)
  ((data :initarg :data :initform "" :accessor text-data)))

(defmethod node-type ((node text-node)) :text)

(defun make-text-node (data)
  (make-instance 'text-node :data data))

;;; ============================================================
;;; Comment node
;;; ============================================================

(defclass comment-node (node)
  ((data :initarg :data :initform "" :accessor comment-data)))

(defmethod node-type ((node comment-node)) :comment)

(defun make-comment-node (data)
  (make-instance 'comment-node :data data))

;;; ============================================================
;;; DocumentType node
;;; ============================================================

(defclass doctype-node (node)
  ((name :initarg :name :initform nil :accessor doctype-name)
   (public-id :initarg :public-id :initform "" :accessor doctype-public-id)
   (system-id :initarg :system-id :initform "" :accessor doctype-system-id)))

(defmethod node-type ((node doctype-node)) :doctype)

(defun make-doctype-node (&key name public-id system-id)
  (make-instance 'doctype-node
                 :name name
                 :public-id (or public-id "")
                 :system-id (or system-id "")))

;;; ============================================================
;;; Tree manipulation
;;; ============================================================

(defun append-child (parent child)
  "Append CHILD to PARENT's children list. Sets parent pointer."
  (setf (node-parent child) parent)
  (vector-push-extend child (node-children parent))
  child)

(defun insert-child-before (parent child reference)
  "Insert CHILD before REFERENCE in PARENT's children. If REFERENCE is NIL, append."
  (if (null reference)
      (append-child parent child)
      (let* ((children (node-children parent))
             (pos (position reference children)))
        (when pos
          ;; Extend array and shift elements
          (vector-push-extend nil children)
          (loop for i from (1- (length children)) downto (1+ pos)
                do (setf (aref children i) (aref children (1- i))))
          (setf (aref children pos) child)
          (setf (node-parent child) parent))))
  child)

(defun remove-child (parent child)
  "Remove CHILD from PARENT's children."
  (let* ((children (node-children parent))
         (pos (position child children)))
    (when pos
      (loop for i from pos below (1- (length children))
            do (setf (aref children i) (aref children (1+ i))))
      (decf (fill-pointer children))
      (setf (node-parent child) nil)))
  child)

(defun last-child (node)
  "Return the last child of NODE, or NIL."
  (let ((children (node-children node)))
    (when (> (length children) 0)
      (aref children (1- (length children))))))

;;; ============================================================
;;; DOM query utilities
;;; ============================================================

(defun child-elements (node)
  "Return a list of child element nodes."
  (loop for child across (node-children node)
        when (typep child 'element-node)
          collect child))

(defun child-elements-by-tag (node tag-name)
  "Return child elements matching TAG-NAME."
  (loop for child across (node-children node)
        when (and (typep child 'element-node)
                  (string-equal (element-tag-name child) tag-name))
          collect child))

(defun first-child-element (node &optional tag-name)
  "Return the first child element, optionally matching TAG-NAME."
  (loop for child across (node-children node)
        when (and (typep child 'element-node)
                  (or (null tag-name)
                      (string-equal (element-tag-name child) tag-name)))
          return child))

(defun get-element-by-id (root id)
  "Depth-first search for an element with the given ID."
  (when (typep root 'element-node)
    (when (string= (element-attribute root "id") id)
      (return-from get-element-by-id root)))
  (loop for child across (node-children root)
        for result = (get-element-by-id child id)
        when result return result))

(defun get-elements-by-tag-name (root tag-name)
  "Collect all descendant elements matching TAG-NAME (case-insensitive)."
  (let ((result '()))
    (labels ((walk (node)
               (when (and (typep node 'element-node)
                          (string-equal (element-tag-name node) tag-name))
                 (push node result))
               (loop for child across (node-children node)
                     do (walk child))))
      (walk root))
    (nreverse result)))

;;; ============================================================
;;; Element classification
;;; ============================================================

(defparameter *void-elements*
  '("area" "base" "br" "col" "embed" "hr" "img" "input"
    "link" "meta" "param" "source" "track" "wbr")
  "HTML void elements that have no end tag and no content.")

(defparameter *raw-text-elements*
  '("script" "style" "xmp" "iframe" "noembed" "noframes" "noscript")
  "Elements whose content is raw text (not parsed as HTML).")

(defun inner-text (node)
  "Extract all text content from NODE and its descendants."
  (with-output-to-string (s)
    (labels ((walk (n)
               (typecase n
                 (text-node (write-string (text-data n) s))
                 (t (loop for child across (node-children n)
                          do (walk child))))))
      (walk node))))
