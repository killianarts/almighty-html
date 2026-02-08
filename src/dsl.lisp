(defpackage #:almighty-html/dsl
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate)
  (:import-from #:almighty-html/element
                #:create-element)
  (:export #:</>
           #:deftag
           #:register-web-components
           #:clear-web-components
           #:define-component))
(in-package #:almighty-html/dsl)

;;; </> macro

(defmacro </> (form)
  "Automatically detect html tags, registered Web Components, and user-defined ALMIGHTY-HTML components.
All other expressions are evaluated as regular Lisp forms.
To create ALMIGHTY-HTML elements within a Lisp form, use the `</>` macro again."
  (detect-elements form))

(defun external-symbol (sym package)
  (multiple-value-bind (s kind)
      (find-symbol (string sym) package)
    (and (eq kind :external) s)))

(defun starts-with-acdash-p (sym)
  (string-equal  "ac-" (subseq (string sym) 0 3)))

(defun detect-component (sym)
  (and (starts-with-acdash-p sym) sym))

(defun detect-elements (form)
  (or (and (consp form)
           (listp (rest form))
           (let* ((head (first form))
                  (tail (rest form))
                  (detected-head (and (symbolp head)
                                      (not (keywordp head))
                                      (or (external-symbol head :almighty-html/web-components)
                                          (external-symbol head :almighty-html/builtin)
                                          (detect-component head)))))
             (and detected-head
                  (cons detected-head (mapcar #'detect-elements tail)))))
      form))

;;; def</> macro

(defmacro def</> (name element-type)
  ;; Use a macro instead of a function to allow semantic indentation, similar to HTML.
  `(defmacro ,name (&body body)
     `(%create-element ,',element-type ,@body)))

(defun %create-element (type &rest body)
  (multiple-value-bind (props children)
      (parse-body body)
    (create-element type props children)))

(defun parse-body (body)
  (cond
    ;; body has props as a normal plist
    ((plist-p (first body))
     (values (first body) (rest body)))
    ;; body has props as an inline plist
    ((keywordp (first body))
     (loop :for thing :on body :by #'cddr
           :for (k v) := thing
           :when (and (keywordp k) v)
             :append (list k v) :into props
           :when (not (keywordp k))
             :return (values props thing)
           :finally (return (values props nil))))
    ;; body has no props
    (t (values nil body))))

(defun plist-p (obj)
  (and (listp obj)
       (evenp (length obj))
       (loop :for (key _) :on obj :by #'cddr
             :always (symbolp key))))

(defmacro deftag (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (def</> ,name ,(make-keyword name))))

(defmacro register-web-components (&rest names)
  (let ((pkg (find-package :almighty-html/web-components)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(mapcan (lambda (name)
                   (let ((sym (intern (string name) pkg)))
                     (list `(deftag ,sym)
                           `(export ',sym ',pkg))))
                 names))))

(defun clear-web-components ()
  (let ((pkg :almighty-html/web-components))
    (do-external-symbols (sym pkg)
      (unintern sym pkg))))

(defmacro define-component (name props &body body)
  "Define an ALMIGHTY-HTML component:
- name: must begin with a tilde (~)
- props: must be declared using &key, &rest, or both
         the `children` key receives the component’s child elements
- body: must return a valid ALMIGHTY-HTML element"
  (unless (starts-with-acdash-p name)
    (error "Component names must start with ac-. Rename ~a." name))
  (unless (or (null props)
              (member '&key props)
              (member '&rest props))
    (error "Component properties must be declared using &key, &rest, or both."))
  (let ((%name (symbolicate '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%name ,props ,@body)
       (def</> ,name (fdefinition ',%name)))))
