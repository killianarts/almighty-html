(defpackage #:almighty-html-test/dsl
  (:use #:cl
        #:rove
        #:almighty-html/dsl)
  (:import-from #:almighty-html/builtin)
  (:import-from #:almighty-html/web-components)
  (:import-from #:almighty-html/element
                #:element-props
                #:element-children))
(in-package #:almighty-html-test/dsl)

(define-component ~comp1 (&key children)
  (almighty-html (div children)))

(deftest detect-elements-test
  (testing "detect-tags"
    (ok (expands '(almighty-html (div div div))
                 '(almighty-html/builtin:div div div)))
    (ok (expands '(almighty-html (div (div div (div))))
                 '(almighty-html/builtin:div
                   (almighty-html/builtin:div
                     div
                     (almighty-html/builtin:div))))))

  (testing "detect-components"
    (ok (expands '(almighty-html (~comp1 (div)))
                 '(~comp1 (almighty-html/builtin:div)))))

  (testing "ignore-malformed-form"
    (ok (expands '(almighty-html (div . div))
                 '(div . div)))
    (ok (expands '(almighty-html ((div)))
                 '((div)))))

  (testing "ignore-cl-form"
    (ok (expands '(almighty-html (labels ((div () "div"))
                         (div)))
                 '(labels ((div () "div"))
                   (div))))))

(deftest dsl-test
  (testing "empty-almighty-html"
    (let ((elm (almighty-html (div))))
      (ok (null (element-props elm)))
      (ok (null (element-children elm)))))
 
  (testing "almighty-html-with-static-props"
    (let ((elm (almighty-html (div :prop1 "value1" :prop2 "value2"))))
      (ok (equal '(:prop1 "value1" :prop2 "value2")
                 (element-props elm)))
      (ok (null (element-children elm)))))
  
  (testing "almighty-html-with-dynamic-props"
    (let* ((props '(:prop1 "value1" :prop2 "value2"))
           (elm (almighty-html (div props))))
      (ok (equal props (element-props elm)))
      (ok (null (element-children elm)))))
  
  (testing "almighty-html-with-children"
    (let ((elm (almighty-html (div
                      "child1"
                      "child2"))))
      (ok (null (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm)))))
  
  (testing "almighty-html-with-static-props-and-children"
    (let ((elm (almighty-html (div :prop1 "value1" :prop2 "value2"
                      "child1"
                      "child2"))))
      (ok (equal '(:prop1 "value1" :prop2 "value2")
                 (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm)))))
  
  (testing "almighty-html-with-dynamic-props-and-children"
    (let* ((props '(:prop1 "value1" :prop2 "value2"))
           (elm (almighty-html (div props
                       "child1"
                       "child2"))))
      (ok (equal props (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm))))))

(register-web-components c1 c2)

(deftest web-components-test
  (testing "register-web-components"
    (ok (expands '(almighty-html (c1 (c2)))
                 '(almighty-html/web-components:c1 (almighty-html/web-components:c2)))))
  (testing "clear-web-components"
    (clear-web-components)
    (ok (expands '(almighty-html (c1 (c2)))
                 '(c1 (c2))))))
