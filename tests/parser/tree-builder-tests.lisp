;;;; t/tree-builder-tests.lisp — Tests for the recursive descent parser

(in-package #:almighty-html/parser/tests)

;;; Helper: parse HTML string and return the document node
(defun parse (html)
  (almighty-html/parser:parse-html html))

;;; Helper: collect tag names depth-first (elements only)
(defun collect-tag-names (node)
  (let ((result '()))
    (labels ((walk (n)
               (when (typep n 'almighty-html/parser:element-node)
                 (push (almighty-html/parser:element-tag-name n) result))
               (loop for child across (almighty-html/parser:node-children n)
                     do (walk child))))
      (walk node))
    (nreverse result)))

;;; Helper: find first element with the given tag name (depth-first)
(defun find-element (root tag)
  (labels ((walk (n)
             (when (and (typep n 'almighty-html/parser:element-node)
                        (string-equal (almighty-html/parser:element-tag-name n) tag))
               (return-from find-element n))
             (loop for child across (almighty-html/parser:node-children n)
                   do (walk child))))
    (walk root)
    nil))

;;; Helper: count descendants with given tag
(defun count-elements (root tag)
  (length (almighty-html/parser:get-elements-by-tag-name root tag)))

;;; ============================================================
;;; Basic parsing
;;; ============================================================

(define-test parser-empty-input ()
  "Empty input produces a document node with no children."
  (let ((doc (parse "")))
    (assert-true (typep doc 'almighty-html/parser:document-node))
    (assert-eql 0 (length (almighty-html/parser:node-children doc)))))

(define-test parser-plain-text ()
  "Plain text produces a document with a text node child."
  (let* ((doc (parse "Hello"))
         (children (almighty-html/parser:node-children doc)))
    (assert-eql 1 (length children))
    (assert-true (typep (aref children 0) 'almighty-html/parser:text-node))
    (assert-equal "Hello" (almighty-html/parser:text-data (aref children 0)))))

(define-test parser-whitespace-text ()
  "Whitespace text is preserved."
  (let* ((doc (parse "   \n\t  "))
         (children (almighty-html/parser:node-children doc)))
    (assert-eql 1 (length children))
    (assert-true (typep (aref children 0) 'almighty-html/parser:text-node))))

;;; ============================================================
;;; DOCTYPE handling
;;; ============================================================

(define-test parser-doctype-html5 ()
  "HTML5 doctype is parsed and attached to the document."
  (let* ((doc (parse "<!DOCTYPE html><html><head></head><body></body></html>"))
         (children (almighty-html/parser:node-children doc))
         (first-child (when (> (length children) 0) (aref children 0))))
    (assert-true (typep first-child 'almighty-html/parser:doctype-node))
    (assert-equal "html" (almighty-html/parser:doctype-name first-child))))

(define-test parser-doctype-case-insensitive ()
  "DOCTYPE keyword is case-insensitive and name is lowercased."
  (let* ((doc (parse "<!DOCTYPE HTML>"))
         (children (almighty-html/parser:node-children doc))
         (dt (find-if (lambda (n) (typep n 'almighty-html/parser:doctype-node))
                      (coerce children 'list))))
    (assert-true (not (null dt)))
    (assert-equal "html" (almighty-html/parser:doctype-name dt))))

;;; ============================================================
;;; Element parsing
;;; ============================================================

(define-test parser-simple-element ()
  "A simple element is parsed correctly."
  (let* ((doc (parse "<div>content</div>"))
         (div (find-element doc "div")))
    (assert-true (not (null div)))
    (assert-equal "content" (almighty-html/parser:inner-text div))))

(define-test parser-nested-elements ()
  "Nested elements maintain proper hierarchy."
  (let* ((doc (parse "<div id=\"outer\"><div id=\"inner\">text</div></div>"))
         (outer (almighty-html/parser:get-element-by-id doc "outer"))
         (inner (almighty-html/parser:get-element-by-id doc "inner")))
    (assert-true (not (null outer)))
    (assert-true (not (null inner)))
    (assert-eq outer (almighty-html/parser:node-parent inner))))

(define-test parser-tag-name-case-insensitive ()
  "Tag names are lowercased."
  (let* ((doc (parse "<DIV>text</DIV>"))
         (div (find-element doc "div")))
    (assert-true (not (null div)))))

(define-test parser-deep-nesting ()
  "Deeply nested elements maintain correct parent-child relationships."
  (let* ((doc (parse "<div><section><article><p><span><em><strong>deep</strong></em></span></p></article></section></div>"))
         (strong (find-element doc "strong")))
    (assert-true (not (null strong)))
    (assert-equal "deep" (almighty-html/parser:inner-text strong))
    (let ((em (almighty-html/parser:node-parent strong)))
      (assert-equal "em" (almighty-html/parser:element-tag-name em))
      (let ((span (almighty-html/parser:node-parent em)))
        (assert-equal "span" (almighty-html/parser:element-tag-name span))
        (let ((p (almighty-html/parser:node-parent span)))
          (assert-equal "p" (almighty-html/parser:element-tag-name p)))))))

(define-test parser-full-html5-structure ()
  "Full HTML5 document structure is parsed correctly."
  (let* ((doc (parse "<html><head><title>Test</title></head><body><p>Hello</p></body></html>"))
         (html-el (find-element doc "html"))
         (head (find-element html-el "head"))
         (body (find-element html-el "body")))
    (assert-true (not (null html-el)))
    (assert-true (not (null head)))
    (assert-true (not (null body)))
    (let ((title (find-element head "title")))
      (assert-equal "Test" (almighty-html/parser:inner-text title)))))

(define-test parser-consecutive-elements ()
  "Multiple sibling elements are all parsed."
  (let* ((doc (parse "<p>First</p><p>Second</p>"))
         (ps (almighty-html/parser:get-elements-by-tag-name doc "p")))
    (assert-eql 2 (length ps))
    (assert-equal "First" (almighty-html/parser:inner-text (first ps)))
    (assert-equal "Second" (almighty-html/parser:inner-text (second ps)))))

(define-test parser-headings ()
  "Heading elements h1-h6 are parsed."
  (let* ((doc (parse "<h1>One</h1><h2>Two</h2><h3>Three</h3>"))
         (h1 (find-element doc "h1"))
         (h2 (find-element doc "h2"))
         (h3 (find-element doc "h3")))
    (assert-equal "One" (almighty-html/parser:inner-text h1))
    (assert-equal "Two" (almighty-html/parser:inner-text h2))
    (assert-equal "Three" (almighty-html/parser:inner-text h3))))

;;; ============================================================
;;; Void elements
;;; ============================================================

(define-test parser-br-element ()
  "<br> is a void element with no children."
  (let* ((doc (parse "<p>Line 1<br>Line 2</p>"))
         (p (find-element doc "p"))
         (br (find-element p "br")))
    (assert-true (not (null br)))
    (assert-eql 0 (length (almighty-html/parser:node-children br)))))

(define-test parser-hr-element ()
  "<hr> is a void element."
  (let* ((doc (parse "<div>Before<hr>After</div>"))
         (hr (find-element doc "hr")))
    (assert-true (not (null hr)))
    (assert-eql 0 (length (almighty-html/parser:node-children hr)))))

(define-test parser-img-element ()
  "<img> is a void element with attributes."
  (let* ((doc (parse "<img src=\"photo.jpg\" alt=\"Photo\">"))
         (img (find-element doc "img")))
    (assert-true (not (null img)))
    (assert-equal "photo.jpg" (almighty-html/parser:element-attribute img "src"))
    (assert-equal "Photo" (almighty-html/parser:element-attribute img "alt"))
    (assert-eql 0 (length (almighty-html/parser:node-children img)))))

(define-test parser-input-element ()
  "<input> is a void element."
  (let* ((doc (parse "<input type=\"text\" name=\"q\" value=\"hello\">"))
         (input (find-element doc "input")))
    (assert-true (not (null input)))
    (assert-equal "text" (almighty-html/parser:element-attribute input "type"))
    (assert-equal "hello" (almighty-html/parser:element-attribute input "value"))))

(define-test parser-self-closing-void ()
  "Self-closing syntax on void elements."
  (let ((doc (parse "<br/><hr/><img src=\"x.png\"/>")))
    (assert-true (find-element doc "br"))
    (assert-true (find-element doc "hr"))
    (assert-true (find-element doc "img"))))

;;; ============================================================
;;; Attributes
;;; ============================================================

(define-test parser-multiple-attributes ()
  "Multiple attributes are preserved on elements."
  (let* ((doc (parse "<div id=\"main\" class=\"box\" data-role=\"container\" hidden>content</div>"))
         (div (almighty-html/parser:get-element-by-id doc "main")))
    (assert-true (not (null div)))
    (assert-equal "box" (almighty-html/parser:element-attribute div "class"))
    (assert-equal "container" (almighty-html/parser:element-attribute div "data-role"))
    (assert-true (assoc "hidden" (almighty-html/parser:element-attributes div) :test #'string=))))

(define-test parser-attribute-name-case-insensitive ()
  "Attribute names are lowercased."
  (let* ((doc (parse "<div CLASS=\"foo\">text</div>"))
         (div (find-element doc "div")))
    (assert-equal "foo" (almighty-html/parser:element-attribute div "class"))))

(define-test parser-single-quoted-attribute ()
  "Single-quoted attribute values."
  (let* ((doc (parse "<div class='container'>text</div>"))
         (div (find-element doc "div")))
    (assert-equal "container" (almighty-html/parser:element-attribute div "class"))))

(define-test parser-unquoted-attribute ()
  "Unquoted attribute values."
  (let* ((doc (parse "<div class=container>text</div>"))
         (div (find-element doc "div")))
    (assert-equal "container" (almighty-html/parser:element-attribute div "class"))))

(define-test parser-boolean-attribute ()
  "Boolean attributes (no value) have empty string value."
  (let* ((doc (parse "<input disabled required>"))
         (input (find-element doc "input")))
    (assert-true (assoc "disabled" (almighty-html/parser:element-attributes input) :test #'string=))
    (assert-true (assoc "required" (almighty-html/parser:element-attributes input) :test #'string=))))

(define-test parser-duplicate-attributes ()
  "Duplicate attributes: first occurrence wins."
  (let* ((doc (parse "<div class=\"first\" class=\"second\">text</div>"))
         (div (find-element doc "div")))
    (assert-equal "first" (almighty-html/parser:element-attribute div "class"))))

(define-test parser-empty-attribute-value ()
  "Empty quoted attribute values."
  (let* ((doc (parse "<div id=\"\">text</div>"))
         (div (find-element doc "div")))
    (assert-equal "" (almighty-html/parser:element-attribute div "id"))))

(define-test parser-entity-in-attribute ()
  "Character references in attribute values."
  (let* ((doc (parse "<a href=\"foo&amp;bar\">link</a>"))
         (a (find-element doc "a")))
    (assert-equal "foo&bar" (almighty-html/parser:element-attribute a "href"))))

;;; ============================================================
;;; Lists
;;; ============================================================

(define-test parser-unordered-list ()
  "<ul> with <li> children."
  (let* ((doc (parse "<ul><li>A</li><li>B</li><li>C</li></ul>"))
         (ul (find-element doc "ul"))
         (lis (almighty-html/parser:child-elements-by-tag ul "li")))
    (assert-eql 3 (length lis))
    (assert-equal "A" (almighty-html/parser:inner-text (first lis)))
    (assert-equal "C" (almighty-html/parser:inner-text (third lis)))))

(define-test parser-ordered-list ()
  "<ol> with <li> children."
  (let* ((doc (parse "<ol><li>First</li><li>Second</li></ol>"))
         (ol (find-element doc "ol"))
         (lis (almighty-html/parser:child-elements-by-tag ol "li")))
    (assert-eql 2 (length lis))))

(define-test parser-definition-list ()
  "<dl> with <dt> and <dd>."
  (let* ((doc (parse "<dl><dt>Term</dt><dd>Definition</dd></dl>"))
         (dl (find-element doc "dl")))
    (assert-true (find-element dl "dt"))
    (assert-true (find-element dl "dd"))))

;;; ============================================================
;;; Inline / formatting elements
;;; ============================================================

(define-test parser-inline-elements ()
  "Inline elements nest properly."
  (let* ((doc (parse "<p><em>emphasized <strong>and bold</strong></em></p>"))
         (p (find-element doc "p"))
         (em (find-element p "em"))
         (strong (find-element em "strong")))
    (assert-true (not (null em)))
    (assert-true (not (null strong)))))

(define-test parser-anchor-element ()
  "<a> element with href attribute."
  (let* ((doc (parse "<a href=\"https://example.com\">Link</a>"))
         (a (find-element doc "a")))
    (assert-true (not (null a)))
    (assert-equal "https://example.com" (almighty-html/parser:element-attribute a "href"))
    (assert-equal "Link" (almighty-html/parser:inner-text a))))

(define-test parser-span-element ()
  "<span> with class attribute."
  (let* ((doc (parse "<span class=\"highlight\">text</span>"))
         (span (find-element doc "span")))
    (assert-true (not (null span)))
    (assert-equal "highlight" (almighty-html/parser:element-attribute span "class"))))

;;; ============================================================
;;; Raw text / RCDATA elements
;;; ============================================================

(define-test parser-style-raw-text ()
  "<style> content is raw text (tags not parsed)."
  (let* ((doc (parse "<style>div { color: red; }</style>"))
         (style (find-element doc "style"))
         (text (almighty-html/parser:inner-text style)))
    (assert-true (search "color: red" text))
    (assert-eql 0 (length (almighty-html/parser:child-elements style)))))

(define-test parser-script-raw-text ()
  "<script> content is raw text."
  (let* ((doc (parse "<script>var x = '<div>';</script>"))
         (script (find-element doc "script"))
         (text (almighty-html/parser:inner-text script)))
    (assert-true (search "<div>" text))
    (assert-eql 0 (length (almighty-html/parser:child-elements script)))))

(define-test parser-title-text ()
  "<title> content is parsed as text."
  (let* ((doc (parse "<title>A &amp; B</title>"))
         (title (find-element doc "title"))
         (text (almighty-html/parser:inner-text title)))
    (assert-equal "A & B" text)))

;;; ============================================================
;;; Tables
;;; ============================================================

(define-test parser-simple-table ()
  "A basic table structure."
  (let* ((doc (parse "<table><tr><td>A</td><td>B</td></tr></table>"))
         (table (find-element doc "table"))
         (tr (find-element table "tr"))
         (tds (almighty-html/parser:child-elements-by-tag tr "td")))
    (assert-true (not (null table)))
    (assert-true (not (null tr)))
    (assert-eql 2 (length tds))
    (assert-equal "A" (almighty-html/parser:inner-text (first tds)))))

(define-test parser-table-with-thead-tbody ()
  "Table with explicit <thead> and <tbody>."
  (let* ((doc (parse "<table><thead><tr><th>H</th></tr></thead><tbody><tr><td>D</td></tr></tbody></table>"))
         (table (find-element doc "table")))
    (assert-true (find-element table "thead"))
    (assert-true (find-element table "tbody"))
    (assert-true (find-element table "th"))
    (assert-true (find-element table "td"))))

;;; ============================================================
;;; Comments
;;; ============================================================

(define-test parser-comment-preserved ()
  "HTML comments are preserved in the DOM."
  (let* ((doc (parse "<div><!-- a comment --></div>"))
         (div (find-element doc "div"))
         (children (almighty-html/parser:node-children div))
         (comment (find-if (lambda (n) (typep n 'almighty-html/parser:comment-node))
                           (coerce children 'list))))
    (assert-true (not (null comment)))
    (assert-equal " a comment " (almighty-html/parser:comment-data comment))))

(define-test parser-comment-at-top-level ()
  "Comments at top level are attached to the document."
  (let* ((doc (parse "<!-- top-level --><div>content</div>"))
         (children (almighty-html/parser:node-children doc))
         (comment (find-if (lambda (n) (typep n 'almighty-html/parser:comment-node))
                           (coerce children 'list))))
    (assert-true (not (null comment)))))

;;; ============================================================
;;; Mixed content
;;; ============================================================

(define-test parser-mixed-text-and-elements ()
  "Text nodes interspersed with elements."
  (let* ((doc (parse "<p>Hello <em>beautiful</em> world</p>"))
         (p (find-element doc "p"))
         (text (almighty-html/parser:inner-text p)))
    (assert-equal "Hello beautiful world" text)))

(define-test parser-whitespace-handling ()
  "Whitespace between elements is preserved as text nodes."
  (let* ((doc (parse "<div> <span>a</span> <span>b</span> </div>"))
         (div (find-element doc "div"))
         (text (almighty-html/parser:inner-text div)))
    (assert-true (search "a" text))
    (assert-true (search "b" text))))

;;; ============================================================
;;; Unclosed elements / error recovery
;;; ============================================================

(define-test parser-unclosed-element ()
  "Unclosed elements are implicitly closed at EOF."
  (let* ((doc (parse "<div>unclosed"))
         (div (find-element doc "div")))
    (assert-true (not (null div)))
    (assert-equal "unclosed" (almighty-html/parser:inner-text div))))

(define-test parser-extra-end-tags ()
  "Extra/unmatched end tags are ignored gracefully."
  (let* ((doc (parse "<div>text</div></div></span>")))
    (assert-eql 1 (count-elements doc "div"))))

;;; ============================================================
;;; Form elements
;;; ============================================================

(define-test parser-form-elements ()
  "<form> with various input types."
  (let* ((doc (parse "<form action=\"/submit\" method=\"post\"><input type=\"text\" name=\"user\"><input type=\"password\" name=\"pass\"><button type=\"submit\">Go</button></form>"))
         (form (find-element doc "form")))
    (assert-true (not (null form)))
    (assert-equal "/submit" (almighty-html/parser:element-attribute form "action"))
    (assert-equal "post" (almighty-html/parser:element-attribute form "method"))
    (assert-eql 2 (count-elements form "input"))
    (assert-true (find-element form "button"))))

;;; ============================================================
;;; Character references
;;; ============================================================

(define-test parser-named-entity-amp ()
  "&amp; resolves to &."
  (let* ((doc (parse "<p>&amp;</p>"))
         (p (find-element doc "p")))
    (assert-equal "&" (almighty-html/parser:inner-text p))))

(define-test parser-named-entity-lt ()
  "&lt; resolves to <."
  (let* ((doc (parse "<p>&lt;</p>"))
         (p (find-element doc "p")))
    (assert-equal "<" (almighty-html/parser:inner-text p))))

(define-test parser-named-entity-gt ()
  "&gt; resolves to >."
  (let* ((doc (parse "<p>&gt;</p>"))
         (p (find-element doc "p")))
    (assert-equal ">" (almighty-html/parser:inner-text p))))

(define-test parser-named-entity-quot ()
  "&quot; resolves to double-quote."
  (let* ((doc (parse "<p>&quot;</p>"))
         (p (find-element doc "p")))
    (assert-equal "\"" (almighty-html/parser:inner-text p))))

(define-test parser-numeric-entity-decimal ()
  "&#65; resolves to A."
  (let* ((doc (parse "<p>&#65;</p>"))
         (p (find-element doc "p")))
    (assert-equal "A" (almighty-html/parser:inner-text p))))

(define-test parser-numeric-entity-hex ()
  "&#x41; resolves to A."
  (let* ((doc (parse "<p>&#x41;</p>"))
         (p (find-element doc "p")))
    (assert-equal "A" (almighty-html/parser:inner-text p))))

(define-test parser-numeric-entity-hex-upper ()
  "&#X41; resolves to A (uppercase X)."
  (let* ((doc (parse "<p>&#X41;</p>"))
         (p (find-element doc "p")))
    (assert-equal "A" (almighty-html/parser:inner-text p))))

(define-test parser-entity-in-text ()
  "Entities are resolved within text content."
  (let* ((doc (parse "<p>a&amp;b</p>"))
         (p (find-element doc "p")))
    (assert-equal "a&b" (almighty-html/parser:inner-text p))))

(define-test parser-bare-ampersand ()
  "Bare ampersand (not a valid reference) passes through."
  (let* ((doc (parse "<p>a & b</p>"))
         (p (find-element doc "p")))
    (assert-equal "a & b" (almighty-html/parser:inner-text p))))

;;; ============================================================
;;; Edge cases
;;; ============================================================

(define-test parser-less-than-in-text ()
  "A lone < followed by non-alpha is treated as text."
  (let* ((doc (parse "<p>1 < 2</p>"))
         (p (find-element doc "p")))
    (assert-equal "1 < 2" (almighty-html/parser:inner-text p))))

(define-test parser-tag-with-newlines ()
  "Tag with newlines in attributes."
  (let* ((doc (parse (format nil "<div~%  id=\"test\"~%  class=\"foo\">text</div>")))
         (div (find-element doc "div")))
    (assert-equal "test" (almighty-html/parser:element-attribute div "id"))
    (assert-equal "foo" (almighty-html/parser:element-attribute div "class"))))

(define-test parser-bogus-processing-instruction ()
  "Invalid markup like <? is treated as a comment."
  (let* ((doc (parse "<?xml version=\"1.0\"?><p>text</p>"))
         (children (coerce (almighty-html/parser:node-children doc) 'list))
         (comment (find-if (lambda (n) (typep n 'almighty-html/parser:comment-node)) children)))
    (assert-true (not (null comment)))))
