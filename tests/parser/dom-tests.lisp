;;;; tests/dom-tests.lisp — Tests for DOM node operations

(in-package #:almighty-html/parser/tests)

;;; ============================================================
;;; Node creation tests
;;; ============================================================

(define-test dom-create-document ()  "A new document has no children."
  (let ((doc (dom:make-document)))
    (assert-eql :document (node-type doc))
    (assert-eql 0 (length (node-children doc)))))

(define-test dom-create-element ()  "Elements are created with tag name and attributes."
  (let ((el (dom:make-element "div" :attributes '(("id" . "main") ("class" . "container")))))
    (assert-eql :element (node-type el))
    (assert-equal "div" (element-tag-name el))
    (assert-equal "main" (element-attribute el "id"))
    (assert-equal "container" (element-attribute el "class"))
    (assert-eql nil (element-attribute el "nonexistent"))))

(define-test dom-create-text ()  "Text nodes hold character data."
  (let ((text (dom:make-text-node "Hello, world!")))
    (assert-eql :text (node-type text))
    (assert-equal "Hello, world!" (text-data text))))

(define-test dom-create-comment ()  "Comment nodes hold comment data."
  (let ((comment (dom:make-comment-node "This is a comment")))
    (assert-eql :comment (node-type comment))
    (assert-equal "This is a comment" (comment-data comment))))

(define-test dom-create-doctype ()  "Doctype nodes hold name, public-id, system-id."
  (let ((dt (dom:make-doctype-node :name "html" :public-id "" :system-id "")))
    (assert-eql :doctype (node-type dt))
    (assert-equal "html" (doctype-name dt))
    (assert-equal "" (doctype-public-id dt))
    (assert-equal "" (doctype-system-id dt))))

;;; ============================================================
;;; Tree manipulation tests
;;; ============================================================

(define-test dom-append-child ()  "Appending a child adds it and sets parent."
  (let ((parent (dom:make-element "div"))
        (child (dom:make-element "span")))
    (dom:append-child parent child)
    (assert-eql 1 (length (node-children parent)))
    (assert-eq child (aref (node-children parent) 0))
    (assert-eq parent (node-parent child))))

(define-test dom-multiple-children ()  "Multiple children are appended in order."
  (let ((parent (dom:make-element "ul"))
        (li1 (dom:make-element "li"))
        (li2 (dom:make-element "li"))
        (li3 (dom:make-element "li")))
    (dom:append-child parent li1)
    (dom:append-child parent li2)
    (dom:append-child parent li3)
    (assert-eql 3 (length (node-children parent)))
    (assert-eq li1 (aref (node-children parent) 0))
    (assert-eq li2 (aref (node-children parent) 1))
    (assert-eq li3 (aref (node-children parent) 2))))

(define-test dom-remove-child ()  "Removing a child removes it and clears parent."
  (let ((parent (dom:make-element "div"))
        (child1 (dom:make-element "p"))
        (child2 (dom:make-element "span")))
    (dom:append-child parent child1)
    (dom:append-child parent child2)
    (dom:remove-child parent child1)
    (assert-eql 1 (length (node-children parent)))
    (assert-eq child2 (aref (node-children parent) 0))
    (assert-eql nil (node-parent child1))))

(define-test dom-insert-before ()  "Inserting before a reference child works correctly."
  (let ((parent (dom:make-element "div"))
        (first (dom:make-element "first"))
        (last (dom:make-element "last"))
        (middle (dom:make-element "middle")))
    (dom:append-child parent first)
    (dom:append-child parent last)
    (dom:insert-child-before parent middle last)
    (assert-eql 3 (length (node-children parent)))
    (assert-eq first (aref (node-children parent) 0))
    (assert-eq middle (aref (node-children parent) 1))
    (assert-eq last (aref (node-children parent) 2))))

;;; ============================================================
;;; DOM query tests
;;; ============================================================

(define-test dom-child-elements ()  "child-elements returns only element children."
  (let ((parent (dom:make-element "div"))
        (text (dom:make-text-node "hello"))
        (child (dom:make-element "span"))
        (comment (dom:make-comment-node "x")))
    (dom:append-child parent text)
    (dom:append-child parent child)
    (dom:append-child parent comment)
    (let ((elems (child-elements parent)))
      (assert-eql 1 (length elems))
      (assert-eq child (first elems)))))

(define-test dom-child-elements-by-tag ()  "child-elements-by-tag filters by tag name."
  (let ((parent (dom:make-element "div"))
        (p1 (dom:make-element "p"))
        (span (dom:make-element "span"))
        (p2 (dom:make-element "p")))
    (dom:append-child parent p1)
    (dom:append-child parent span)
    (dom:append-child parent p2)
    (let ((ps (child-elements-by-tag parent "p")))
      (assert-eql 2 (length ps))
      (assert-eq p1 (first ps))
      (assert-eq p2 (second ps)))))

(define-test dom-get-element-by-id ()  "get-element-by-id finds nested elements."
  (let ((doc (dom:make-document))
        (html (dom:make-element "html"))
        (body (dom:make-element "body"))
        (target (dom:make-element "div" :attributes '(("id" . "target")))))
    (dom:append-child doc html)
    (dom:append-child html body)
    (dom:append-child body target)
    (assert-eq target (get-element-by-id doc "target"))
    (assert-eql nil (get-element-by-id doc "nonexistent"))))

(define-test dom-get-elements-by-tag-name ()  "get-elements-by-tag-name finds all matching descendants."
  (let ((doc (dom:make-document))
        (html (dom:make-element "html"))
        (body (dom:make-element "body"))
        (div1 (dom:make-element "div"))
        (div2 (dom:make-element "div"))
        (span (dom:make-element "span")))
    (dom:append-child doc html)
    (dom:append-child html body)
    (dom:append-child body div1)
    (dom:append-child div1 div2)
    (dom:append-child body span)
    (let ((divs (get-elements-by-tag-name doc "div")))
      (assert-eql 2 (length divs)))))

(define-test dom-inner-text ()  "inner-text extracts all text content."
  (let ((div (dom:make-element "div"))
        (text1 (dom:make-text-node "Hello "))
        (span (dom:make-element "span"))
        (text2 (dom:make-text-node "world")))
    (dom:append-child div text1)
    (dom:append-child div span)
    (dom:append-child span text2)
    (assert-equal "Hello world" (inner-text div))))

(define-test dom-last-child ()  "last-child returns the final child node."
  (let ((parent (dom:make-element "div"))
        (child1 (dom:make-element "a"))
        (child2 (dom:make-element "b")))
    (assert-eql nil (dom:last-child parent))
    (dom:append-child parent child1)
    (assert-eq child1 (dom:last-child parent))
    (dom:append-child parent child2)
    (assert-eq child2 (dom:last-child parent))))

(define-test dom-set-attribute ()  "Setting attributes works for new and existing attributes."
  (let ((el (dom:make-element "div")))
    (setf (element-attribute el "class") "foo")
    (assert-equal "foo" (element-attribute el "class"))
    (setf (element-attribute el "class") "bar")
    (assert-equal "bar" (element-attribute el "class"))))
