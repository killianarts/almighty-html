;;;; t/integration-tests.lisp — End-to-end parsing and serialization tests

(in-package #:almighty-html/parser/tests)

;;; ============================================================
;;; Full document parsing
;;; ============================================================

(define-test integration-full-html5-document ()
  "Parse a complete HTML5 document."
  (let* ((html "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <title>Test Page</title>
</head>
<body>
  <h1>Hello World</h1>
  <p>This is a <em>test</em> page.</p>
</body>
</html>")
         (doc (parse html)))
    ;; Doctype present
    (let ((children (coerce (almighty-html/parser:node-children doc) 'list)))
      (assert-true (find-if (lambda (n) (typep n 'almighty-html/parser:doctype-node)) children)))
    ;; Structure is correct
    (let ((html-el (find-element doc "html")))
      (assert-true (not (null html-el)))
      (assert-equal "en" (almighty-html/parser:element-attribute html-el "lang")))
    ;; Title parsed
    (let ((title (find-element doc "title")))
      (assert-equal "Test Page" (almighty-html/parser:inner-text title)))
    ;; Body content
    (let ((h1 (find-element doc "h1")))
      (assert-equal "Hello World" (almighty-html/parser:inner-text h1)))
    (let ((p (find-element doc "p")))
      (assert-true (search "test" (almighty-html/parser:inner-text p))))
    (let ((em (find-element doc "em")))
      (assert-equal "test" (almighty-html/parser:inner-text em)))))

;;; ============================================================
;;; Serialization round-trip
;;; ============================================================

(define-test integration-serialize-basic ()
  "Serialized output contains expected elements."
  (let* ((doc (parse "<div><p>Hello</p></div>"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (search "<div>" output))
    (assert-true (search "<p>" output))
    (assert-true (search "Hello" output))
    (assert-true (search "</p>" output))
    (assert-true (search "</div>" output))))

(define-test integration-serialize-void-elements ()
  "Void elements are serialized without closing tags."
  (let* ((doc (parse "<p>Line 1<br>Line 2</p>"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (search "<br>" output))
    (assert-true (not (search "</br>" output)))))

(define-test integration-serialize-attributes ()
  "Attributes are serialized correctly."
  (let* ((doc (parse "<a href=\"https://example.com\" class=\"link\">Click</a>"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (search "href=\"https://example.com\"" output))
    (assert-true (search "class=\"link\"" output))))

(define-test integration-serialize-escaping ()
  "Special characters are escaped in serialized output."
  (let* ((doc (parse "<p>1 &lt; 2 &amp; 3 &gt; 0</p>"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (search "&lt;" output))
    (assert-true (search "&amp;" output))
    (assert-true (search "&gt;" output))))

(define-test integration-serialize-doctype ()
  "DOCTYPE is serialized."
  (let* ((doc (parse "<!DOCTYPE html><html><body></body></html>"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (search "<!DOCTYPE html>" output))))

(define-test integration-serialize-comment ()
  "Comments are serialized."
  (let* ((doc (parse "<div><!-- hello --></div>"))
         (output (almighty-html/parser:serialize-node doc)))
    (assert-true (search "<!--" output))
    (assert-true (search "-->" output))))

;;; ============================================================
;;; Complex real-world patterns
;;; ============================================================

(define-test integration-nav-bar ()
  "A typical navigation bar structure."
  (let* ((doc (parse "<nav><ul><li><a href=\"/\">Home</a></li><li><a href=\"/about\">About</a></li><li><a href=\"/contact\">Contact</a></li></ul></nav>"))
         (nav (find-element doc "nav"))
         (links (almighty-html/parser:get-elements-by-tag-name nav "a")))
    (assert-eql 3 (length links))
    (assert-equal "/" (almighty-html/parser:element-attribute (first links) "href"))
    (assert-equal "Home" (almighty-html/parser:inner-text (first links)))
    (assert-equal "Contact" (almighty-html/parser:inner-text (third links)))))

(define-test integration-article-structure ()
  "Article with header, paragraphs, and footer."
  (let* ((html "<article>
  <header><h1>Title</h1><time>2025-01-01</time></header>
  <p>First paragraph.</p>
  <p>Second paragraph with <a href=\"#\">a link</a>.</p>
  <footer><small>Copyright 2025</small></footer>
</article>")
         (doc (parse html))
         (article (find-element doc "article")))
    (assert-true (not (null article)))
    (assert-true (find-element article "header"))
    (assert-true (find-element article "footer"))
    (assert-eql 2 (count-elements article "p"))
    (let ((h1 (find-element article "h1")))
      (assert-equal "Title" (almighty-html/parser:inner-text h1)))))

(define-test integration-complex-table ()
  "A table with caption, header, body, and footer."
  (let* ((html "<table>
  <caption>Sales Data</caption>
  <thead><tr><th>Product</th><th>Revenue</th></tr></thead>
  <tbody>
    <tr><td>Widget A</td><td>$1000</td></tr>
    <tr><td>Widget B</td><td>$2000</td></tr>
  </tbody>
  <tfoot><tr><td>Total</td><td>$3000</td></tr></tfoot>
</table>")
         (doc (parse html))
         (table (find-element doc "table")))
    (assert-true (find-element table "caption"))
    (assert-true (find-element table "thead"))
    (assert-true (find-element table "tbody"))
    (assert-true (find-element table "tfoot"))
    (assert-eql 2 (count-elements (find-element table "thead") "th"))
    (assert-eql 4 (count-elements (find-element table "tbody") "td"))))

(define-test integration-form ()
  "A form with various input types."
  (let* ((html "<form action=\"/login\" method=\"post\">
  <div>
    <label for=\"user\">Username:</label>
    <input type=\"text\" id=\"user\" name=\"username\">
  </div>
  <div>
    <label for=\"pass\">Password:</label>
    <input type=\"password\" id=\"pass\" name=\"password\">
  </div>
  <button type=\"submit\">Log In</button>
</form>")
         (doc (parse html))
         (form (find-element doc "form")))
    (assert-equal "/login" (almighty-html/parser:element-attribute form "action"))
    (assert-eql 2 (count-elements form "input"))
    (assert-eql 2 (count-elements form "label"))
    (let ((btn (find-element form "button")))
      (assert-equal "Log In" (almighty-html/parser:inner-text btn)))))

(define-test integration-nested-lists ()
  "Nested lists maintain correct structure."
  (let* ((html "<ul>
  <li>Item 1
    <ul>
      <li>Sub A</li>
      <li>Sub B</li>
    </ul>
  </li>
  <li>Item 2</li>
</ul>")
         (doc (parse html))
         (outer-ul (find-element doc "ul")))
    (assert-true (not (null outer-ul)))
    (assert-eql 2 (count-elements doc "ul"))
    (assert-eql 4 (count-elements doc "li"))))

;;; ============================================================
;;; Entity handling in context
;;; ============================================================

(define-test integration-entities-in-content ()
  "Named entities are resolved in text content."
  (let* ((doc (parse "<p>Tom &amp; Jerry — Best &copy; Duo</p>"))
         (p (find-element doc "p"))
         (text (almighty-html/parser:inner-text p)))
    (assert-true (search "&" text))
    (assert-true (search "©" text))))

(define-test integration-entities-in-attributes ()
  "Entities are resolved in attribute values."
  (let* ((doc (parse "<a href=\"/search?a=1&amp;b=2\">Search</a>"))
         (a (find-element doc "a")))
    (assert-equal "/search?a=1&b=2" (almighty-html/parser:element-attribute a "href"))))

(define-test integration-numeric-entities ()
  "Numeric character references in content."
  (let* ((doc (parse "<p>&#169; &#x2603;</p>"))
         (p (find-element doc "p"))
         (text (almighty-html/parser:inner-text p)))
    (assert-true (search "©" text))
    (assert-true (search "☃" text))))

;;; ============================================================
;;; Script and style content integrity
;;; ============================================================

(define-test integration-script-preserves-content ()
  "Script content including HTML-like strings is preserved verbatim."
  (let* ((doc (parse "<script>if (a < b && c > d) { document.write('<p>x</p>'); }</script>"))
         (script (find-element doc "script"))
         (text (almighty-html/parser:inner-text script)))
    (assert-true (search "<p>x</p>" text))
    (assert-true (search "< b" text))))

(define-test integration-style-preserves-selectors ()
  "CSS selectors and rules are preserved in <style>."
  (let* ((doc (parse "<style>body > .main { font-size: 16px; } a:hover { color: red; }</style>"))
         (style (find-element doc "style"))
         (text (almighty-html/parser:inner-text style)))
    (assert-true (search "body > .main" text))
    (assert-true (search "a:hover" text))))

;;; ============================================================
;;; DOM query API on parsed documents
;;; ============================================================

(define-test integration-get-element-by-id ()
  "get-element-by-id works on parsed documents."
  (let* ((doc (parse "<div id=\"a\"><span id=\"b\">hello</span></div>"))
         (a (almighty-html/parser:get-element-by-id doc "a"))
         (b (almighty-html/parser:get-element-by-id doc "b")))
    (assert-true (not (null a)))
    (assert-true (not (null b)))
    (assert-equal "div" (almighty-html/parser:element-tag-name a))
    (assert-equal "span" (almighty-html/parser:element-tag-name b))
    (assert-eq a (almighty-html/parser:node-parent b))))

(define-test integration-get-elements-by-tag ()
  "get-elements-by-tag-name works on parsed documents."
  (let* ((doc (parse "<div><p>A</p><p>B</p><div><p>C</p></div></div>"))
         (ps (almighty-html/parser:get-elements-by-tag-name doc "p")))
    (assert-eql 3 (length ps))))

(define-test integration-inner-text-complex ()
  "inner-text extracts all text from complex structure."
  (let* ((doc (parse "<div>Hello <span>beautiful <em>world</em></span>!</div>"))
         (div (find-element doc "div"))
         (text (almighty-html/parser:inner-text div)))
    (assert-equal "Hello beautiful world!" text)))

;;; ============================================================
;;; Edge cases
;;; ============================================================

(define-test integration-only-comment ()
  "Document with only a comment."
  (let* ((doc (parse "<!-- just a comment -->"))
         (children (coerce (almighty-html/parser:node-children doc) 'list)))
    (assert-true (find-if (lambda (n) (typep n 'almighty-html/parser:comment-node)) children))))

(define-test integration-only-doctype ()
  "Document with only a DOCTYPE."
  (let* ((doc (parse "<!DOCTYPE html>"))
         (children (coerce (almighty-html/parser:node-children doc) 'list)))
    (assert-true (find-if (lambda (n) (typep n 'almighty-html/parser:doctype-node)) children))))

;;; ============================================================
;;; Big structure stress test
;;; ============================================================

(define-test integration-many-siblings ()
  "Parser handles many sibling elements."
  (let* ((items (with-output-to-string (s)
                  (write-string "<ul>" s)
                  (loop for i from 1 to 100
                        do (format s "<li>Item ~d</li>" i))
                  (write-string "</ul>" s)))
         (doc (parse items))
         (lis (almighty-html/parser:get-elements-by-tag-name doc "li")))
    (assert-eql 100 (length lis))))

(define-test integration-deeply-nested ()
  "Parser handles deep nesting."
  (let* ((depth 50)
         (html (with-output-to-string (s)
                 (loop repeat depth do (write-string "<div>" s))
                 (write-string "deep" s)
                 (loop repeat depth do (write-string "</div>" s))))
         (doc (parse html))
         (divs (almighty-html/parser:get-elements-by-tag-name doc "div")))
    (assert-eql depth (length divs))))
