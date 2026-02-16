;;;; t/html-file-tests.lisp — Tests parsing real-world HTML files from t/html/

(in-package #:almighty-html/parser/tests)

;;; ============================================================
;;; Helpers for file-based tests
;;; ============================================================

(defun test-html-dir ()
  "Return the path to the t/html/ directory."
  (asdf:system-relative-pathname :almighty-html/parser/tests "tests/html/"))

(defun read-html-file (filename)
  "Read an HTML file from the t/html/ directory and return its contents as a string."
  (let ((path (merge-pathnames filename (test-html-dir))))
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (let ((contents (make-string (file-length stream))))
        (read-sequence contents stream)
        ;; Trim trailing null characters from the buffer
        (string-right-trim '(#\Null) contents)))))

(defun parse-html-file (filename)
  "Parse an HTML file from the t/html/ directory and return the document node."
  (parse (read-html-file filename)))

(defun find-all-elements (root tag)
  "Return all elements with the given tag name."
  (almighty-html/parser:get-elements-by-tag-name root tag))

;;; ============================================================
;;; Catalyst.html — Dashboard page
;;; ============================================================

(define-test html-file-catalyst-parses ()
  "Catalyst.html parses without error and produces a valid document."
  (let ((doc (parse-html-file "Catalyst.html")))
    (assert-true (typep doc 'almighty-html/parser:document-node))
    (assert-true (find-element doc "html"))
    (assert-true (find-element doc "head"))
    (assert-true (find-element doc "body"))))

(define-test html-file-catalyst-doctype ()
  "Catalyst.html has an HTML5 doctype."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (children (coerce (almighty-html/parser:node-children doc) 'list))
         (dt (find-if (lambda (n) (typep n 'almighty-html/parser:doctype-node)) children)))
    (assert-true (not (null dt)))
    (assert-equal "html" (almighty-html/parser:doctype-name dt))))

(define-test html-file-catalyst-html-lang ()
  "Catalyst.html has lang=\"en\" on the <html> element."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (html-el (find-element doc "html")))
    (assert-equal "en" (almighty-html/parser:element-attribute html-el "lang"))))

(define-test html-file-catalyst-title ()
  "Catalyst.html has a title element in the head."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (title (find-element doc "title")))
    (assert-true (not (null title)))
    (assert-true (> (length (almighty-html/parser:inner-text title)) 0))))

(define-test html-file-catalyst-nav ()
  "Catalyst.html contains a nav element with links."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (navs (find-all-elements doc "nav")))
    (assert-true (> (length navs) 0))
    (let ((links (find-all-elements (first navs) "a")))
      (assert-true (> (length links) 0)))))

(define-test html-file-catalyst-links-have-href ()
  "Links in Catalyst.html have href attributes."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (links (find-all-elements doc "a")))
    (assert-true (> (length links) 0))
    (let ((with-href (remove-if-not
                      (lambda (a) (almighty-html/parser:element-attribute a "href"))
                      links)))
      (assert-true (> (length with-href) 0)))))

(define-test html-file-catalyst-svg-elements ()
  "Catalyst.html contains SVG elements (used for icons)."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (svgs (find-all-elements doc "svg")))
    (assert-true (> (length svgs) 0))))

(define-test html-file-catalyst-data-attributes ()
  "Catalyst.html uses data-slot attributes on elements."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (all-elements (find-all-elements doc "span")))
    (assert-true (some (lambda (el)
                         (almighty-html/parser:element-attribute el "data-slot"))
                       all-elements))))

(define-test html-file-catalyst-aria-attributes ()
  "Catalyst.html uses aria-label attributes for accessibility."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (all-links (find-all-elements doc "a")))
    (assert-true (some (lambda (el)
                         (almighty-html/parser:element-attribute el "aria-label"))
                       all-links))))

(define-test html-file-catalyst-serializes ()
  "Catalyst.html can be serialized back to HTML without error."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (stringp output))
    (assert-true (> (length output) 0))
    (assert-true (search "<html" output))
    (assert-true (search "</html>" output))))

;;; ============================================================
;;; Events - Catalyst.html — Event listing page
;;; ============================================================

(define-test html-file-events-parses ()
  "Events page parses without error."
  (let ((doc (parse-html-file "Events - Catalyst.html")))
    (assert-true (typep doc 'almighty-html/parser:document-node))
    (assert-true (find-element doc "html"))
    (assert-true (find-element doc "body"))))

(define-test html-file-events-has-heading ()
  "Events page has an h1 heading."
  (let* ((doc (parse-html-file "Events - Catalyst.html"))
         (h1s (find-all-elements doc "h1")))
    (assert-true (> (length h1s) 0))))

(define-test html-file-events-has-search-input ()
  "Events page has a search input."
  (let* ((doc (parse-html-file "Events - Catalyst.html"))
         (inputs (find-all-elements doc "input")))
    (assert-true (> (length inputs) 0))
    (assert-true (some (lambda (input)
                         (let ((ph (almighty-html/parser:element-attribute input "placeholder")))
                           (and ph (search "earch" ph))))
                       inputs))))

(define-test html-file-events-has-links ()
  "Events page has multiple links (event entries)."
  (let* ((doc (parse-html-file "Events - Catalyst.html"))
         (links (find-all-elements doc "a")))
    (assert-true (> (length links) 10))))

(define-test html-file-events-nav-structure ()
  "Events page shares the same nav sidebar structure."
  (let* ((doc (parse-html-file "Events - Catalyst.html"))
         (navs (find-all-elements doc "nav")))
    (assert-true (> (length navs) 0))
    (let ((nav-links (find-all-elements (first navs) "a")))
      (assert-true (> (length nav-links) 0)))))

(define-test html-file-events-serializes ()
  "Events page can be serialized back to HTML."
  (let* ((doc (parse-html-file "Events - Catalyst.html"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (> (length output) 0))
    (assert-true (search "</html>" output))))

;;; ============================================================
;;; Orders - Catalyst.html — Table-heavy page
;;; ============================================================

(define-test html-file-orders-parses ()
  "Orders page parses without error."
  (let ((doc (parse-html-file "Orders - Catalyst.html")))
    (assert-true (typep doc 'almighty-html/parser:document-node))
    (assert-true (find-element doc "html"))
    (assert-true (find-element doc "body"))))

(define-test html-file-orders-has-table ()
  "Orders page contains a table element."
  (let* ((doc (parse-html-file "Orders - Catalyst.html"))
         (tables (find-all-elements doc "table")))
    (assert-true (> (length tables) 0))))

(define-test html-file-orders-table-has-thead ()
  "Orders table has a proper thead with th headers."
  (let* ((doc (parse-html-file "Orders - Catalyst.html"))
         (table (find-element doc "table"))
         (thead (find-element table "thead"))
         (ths (find-all-elements thead "th")))
    (assert-true (not (null thead)))
    (assert-true (>= (length ths) 3))))

(define-test html-file-orders-table-has-tbody ()
  "Orders table has a tbody with data rows."
  (let* ((doc (parse-html-file "Orders - Catalyst.html"))
         (table (find-element doc "table"))
         (tbody (find-element table "tbody"))
         (rows (find-all-elements tbody "tr")))
    (assert-true (not (null tbody)))
    (assert-true (>= (length rows) 3))))

(define-test html-file-orders-table-cells ()
  "Orders table rows contain td cells with content."
  (let* ((doc (parse-html-file "Orders - Catalyst.html"))
         (table (find-element doc "table"))
         (tbody (find-element table "tbody"))
         (tds (find-all-elements tbody "td")))
    (assert-true (>= (length tds) 10))))

(define-test html-file-orders-has-heading ()
  "Orders page has an h1 heading."
  (let* ((doc (parse-html-file "Orders - Catalyst.html"))
         (h1s (find-all-elements doc "h1")))
    (assert-true (> (length h1s) 0))))

(define-test html-file-orders-serializes ()
  "Orders page serializes and the output contains table structure."
  (let* ((doc (parse-html-file "Orders - Catalyst.html"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (search "<table" output))
    (assert-true (search "<thead" output))
    (assert-true (search "<tbody" output))
    (assert-true (search "</table>" output))))

;;; ============================================================
;;; Settings - Catalyst.html — Form-heavy page
;;; ============================================================

(define-test html-file-settings-parses ()
  "Settings page parses without error."
  (let ((doc (parse-html-file "Settings - Catalyst.html")))
    (assert-true (typep doc 'almighty-html/parser:document-node))
    (assert-true (find-element doc "html"))
    (assert-true (find-element doc "body"))))

(define-test html-file-settings-has-form ()
  "Settings page contains a form element."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (forms (find-all-elements doc "form")))
    (assert-true (> (length forms) 0))))

(define-test html-file-settings-form-has-method ()
  "Settings form has a method attribute (post)."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (form (find-element doc "form"))
         (method (almighty-html/parser:element-attribute form "method")))
    (assert-true (not (null method)))
    (assert-equal "post" (string-downcase method))))

(define-test html-file-settings-has-inputs ()
  "Settings page has multiple input fields."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (inputs (find-all-elements doc "input")))
    (assert-true (>= (length inputs) 3))))

(define-test html-file-settings-has-labels ()
  "Settings page has label elements for form fields."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (labels (find-all-elements doc "label")))
    (assert-true (>= (length labels) 1))))

(define-test html-file-settings-has-textarea ()
  "Settings page contains a textarea element."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (textareas (find-all-elements doc "textarea")))
    (assert-true (> (length textareas) 0))))

(define-test html-file-settings-has-sections ()
  "Settings page uses section elements for form grouping."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (sections (find-all-elements doc "section")))
    (assert-true (>= (length sections) 2))))

(define-test html-file-settings-input-types ()
  "Settings page has inputs with various type attributes."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (inputs (find-all-elements doc "input"))
         (types (remove nil (mapcar (lambda (inp)
                                      (almighty-html/parser:element-attribute inp "type"))
                                    inputs))))
    (assert-true (member "email" types :test #'string-equal))
    (assert-true (member "checkbox" types :test #'string-equal))))

(define-test html-file-settings-input-placeholders ()
  "Settings page inputs have placeholder attributes."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (inputs (find-all-elements doc "input")))
    (assert-true (some (lambda (inp)
                         (almighty-html/parser:element-attribute inp "placeholder"))
                       inputs))))

(define-test html-file-settings-serializes ()
  "Settings page can be serialized back to HTML."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (search "<form" output))
    (assert-true (search "<input" output))
    (assert-true (search "</form>" output))))

;;; ============================================================
;;; Cross-file structural consistency
;;; ============================================================

(define-test html-file-all-have-doctype ()
  "All HTML files have an HTML5 doctype."
  (dolist (file '("Catalyst.html"
                  "Events - Catalyst.html"
                  "Orders - Catalyst.html"
                  "Settings - Catalyst.html"))
    (let* ((doc (parse-html-file file))
           (children (coerce (almighty-html/parser:node-children doc) 'list))
           (dt (find-if (lambda (n) (typep n 'almighty-html/parser:doctype-node)) children)))
      (assert-true dt (format nil "~a should have a doctype" file))
      (assert-equal "html" (almighty-html/parser:doctype-name dt)
                    (format nil "~a doctype should be 'html'" file)))))

(define-test html-file-all-have-nav ()
  "All pages share a navigation sidebar."
  (dolist (file '("Catalyst.html"
                  "Events - Catalyst.html"
                  "Orders - Catalyst.html"
                  "Settings - Catalyst.html"))
    (let* ((doc (parse-html-file file))
           (navs (find-all-elements doc "nav")))
      (assert-true (> (length navs) 0)
                   (format nil "~a should have a nav element" file)))))

(define-test html-file-all-have-head-and-body ()
  "All pages have proper head and body elements."
  (dolist (file '("Catalyst.html"
                  "Events - Catalyst.html"
                  "Orders - Catalyst.html"
                  "Settings - Catalyst.html"))
    (let* ((doc (parse-html-file file)))
      (assert-true (find-element doc "head")
                   (format nil "~a should have a head" file))
      (assert-true (find-element doc "body")
                   (format nil "~a should have a body" file)))))

(define-test html-file-all-serialize-roundtrip ()
  "All pages can be parsed and re-serialized without error.
The re-serialized output can be parsed again."
  (dolist (file '("Catalyst.html"
                  "Events - Catalyst.html"
                  "Orders - Catalyst.html"
                  "Settings - Catalyst.html"))
    (let* ((doc (parse-html-file file))
           (output (almighty-html/parser:serialize-node doc))
           (doc2 (parse output)))
      (assert-true (typep doc2 'almighty-html/parser:document-node)
                   (format nil "~a re-parse should produce a document" file))
      (assert-true (find-element doc2 "html")
                   (format nil "~a re-parse should have html element" file))
      (assert-true (find-element doc2 "body")
                   (format nil "~a re-parse should have body element" file)))))

;;; ============================================================
;;; Stress: large file parsing performance sanity check
;;; ============================================================

(define-test html-file-orders-element-count ()
  "Orders page (largest file at ~120KB) produces many elements."
  (let* ((doc (parse-html-file "Orders - Catalyst.html"))
         (all-divs (find-all-elements doc "div"))
         (all-spans (find-all-elements doc "span")))
    (assert-true (> (length all-divs) 40))
    (assert-true (> (length all-spans) 10))))

(define-test html-file-settings-complex-attributes ()
  "Settings page elements preserve complex CSS class attributes."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (form (find-element doc "form"))
         (class (almighty-html/parser:element-attribute form "class")))
    (assert-true (and class (> (length class) 0)))))

;;; ============================================================
;;; Serialization: exact round-trip for well-formed input
;;; ============================================================

(define-test html-file-serialize-exact-doctype ()
  "A well-formed full document serializes back to identical input."
  (let ((html "<!DOCTYPE html><html><head><title>Test</title></head><body><p>Hello</p></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-attributes ()
  "Attributes are preserved exactly through parse-serialize."
  (let ((html "<!DOCTYPE html><html><head></head><body><a href=\"/foo\" class=\"bar\">link</a></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-void-elements ()
  "Void elements round-trip without spurious closing tags."
  (let ((html "<!DOCTYPE html><html><head><meta charset=\"utf-8\"></head><body><br><hr></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-entities ()
  "Character entities in text round-trip through escaping."
  (let ((html "<!DOCTYPE html><html><head></head><body><p>1 &lt; 2 &amp; 3 &gt; 0</p></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-entity-in-attr ()
  "Character entities in attribute values round-trip correctly."
  (let ((html "<!DOCTYPE html><html><head></head><body><a href=\"/s?a=1&amp;b=2\">go</a></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-comment ()
  "Comments round-trip exactly."
  (let ((html "<!DOCTYPE html><html><head></head><body><!-- hello world --></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-nested ()
  "Deeply nested structures round-trip exactly."
  (let ((html "<!DOCTYPE html><html><head></head><body><div><ul><li><a href=\"#\">link</a></li></ul></div></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-multiple-attrs ()
  "Multiple attributes on an element round-trip exactly."
  (let ((html "<!DOCTYPE html><html><head></head><body><input type=\"text\" id=\"name\" placeholder=\"Enter name\" required=\"\"></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-empty-attr ()
  "Empty attribute values (boolean attributes) round-trip."
  (let ((html "<!DOCTYPE html><html><head></head><body><input disabled=\"\"></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-mixed-content ()
  "Mixed text and inline elements round-trip exactly."
  (let ((html "<!DOCTYPE html><html><head></head><body><p>Hello <em>beautiful</em> <strong>world</strong>!</p></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-table ()
  "A table structure round-trips exactly."
  (let ((html "<!DOCTYPE html><html><head></head><body><table><thead><tr><th>A</th><th>B</th></tr></thead><tbody><tr><td>1</td><td>2</td></tr></tbody></table></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

(define-test html-file-serialize-exact-form ()
  "A form with inputs round-trips exactly."
  (let ((html "<!DOCTYPE html><html><head></head><body><form method=\"post\" action=\"/submit\"><label>Name</label><input type=\"text\" name=\"n\"><textarea></textarea><button>Go</button></form></body></html>"))
    (assert-equal html (almighty-html/parser:serialize-node (parse html)))))

;;; ============================================================
;;; Serialization: idempotency (serialize→parse→serialize)
;;; ============================================================

(define-test html-file-serialize-idempotent-catalyst ()
  "Catalyst.html serialization is idempotent: serialize(parse(serialize(parse(input)))) = serialize(parse(input))."
  (let* ((doc (parse-html-file "Catalyst.html"))
         (output1 (almighty-html/parser:serialize-node doc))
         (output2 (almighty-html/parser:serialize-node (parse output1))))
    (assert-equal output1 output2)))

(define-test html-file-serialize-idempotent-events ()
  "Events page serialization is idempotent."
  (let* ((doc (parse-html-file "Events - Catalyst.html"))
         (output1 (almighty-html/parser:serialize-node doc))
         (output2 (almighty-html/parser:serialize-node (parse output1))))
    (assert-equal output1 output2)))

(define-test html-file-serialize-idempotent-orders ()
  "Orders page serialization is idempotent."
  (let* ((doc (parse-html-file "Orders - Catalyst.html"))
         (output1 (almighty-html/parser:serialize-node doc))
         (output2 (almighty-html/parser:serialize-node (parse output1))))
    (assert-equal output1 output2)))

(define-test html-file-serialize-idempotent-settings ()
  "Settings page serialization is idempotent."
  (let* ((doc (parse-html-file "Settings - Catalyst.html"))
         (output1 (almighty-html/parser:serialize-node doc))
         (output2 (almighty-html/parser:serialize-node (parse output1))))
    (assert-equal output1 output2)))

;;; ============================================================
;;; Serialization: structural preservation through round-trip
;;; ============================================================

(define-test html-file-serialize-preserves-element-count ()
  "Parsing serialized output preserves the number of elements for each file."
  (dolist (file '("Catalyst.html"
                  "Events - Catalyst.html"
                  "Orders - Catalyst.html"
                  "Settings - Catalyst.html"))
    (let* ((doc1 (parse-html-file file))
           (output (almighty-html/parser:serialize-node doc1))
           (doc2 (parse output)))
      (assert-eql (length (find-all-elements doc1 "div"))
                  (length (find-all-elements doc2 "div"))
                  (format nil "~a div count mismatch after round-trip" file))
      (assert-eql (length (find-all-elements doc1 "a"))
                  (length (find-all-elements doc2 "a"))
                  (format nil "~a anchor count mismatch after round-trip" file))
      (assert-eql (length (find-all-elements doc1 "span"))
                  (length (find-all-elements doc2 "span"))
                  (format nil "~a span count mismatch after round-trip" file)))))

(define-test html-file-serialize-preserves-attributes ()
  "Attributes on key elements survive parse→serialize→parse."
  (dolist (file '("Catalyst.html"
                  "Events - Catalyst.html"
                  "Orders - Catalyst.html"
                  "Settings - Catalyst.html"))
    (let* ((doc1 (parse-html-file file))
           (output (almighty-html/parser:serialize-node doc1))
           (doc2 (parse output))
           (html1 (find-element doc1 "html"))
           (html2 (find-element doc2 "html")))
      (assert-equal (almighty-html/parser:element-attribute html1 "lang")
                    (almighty-html/parser:element-attribute html2 "lang")
                    (format nil "~a lang attr mismatch after round-trip" file)))))

(define-test html-file-serialize-preserves-text-content ()
  "Text content of key elements survives parse→serialize→parse."
  (dolist (file '("Catalyst.html"
                  "Events - Catalyst.html"
                  "Orders - Catalyst.html"
                  "Settings - Catalyst.html"))
    (let* ((doc1 (parse-html-file file))
           (output (almighty-html/parser:serialize-node doc1))
           (doc2 (parse output))
           (title1 (find-element doc1 "title"))
           (title2 (find-element doc2 "title")))
      (assert-equal (almighty-html/parser:inner-text title1)
                    (almighty-html/parser:inner-text title2)
                    (format nil "~a title text mismatch after round-trip" file)))))
