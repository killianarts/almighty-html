;;;; parser.lisp — Recursive descent HTML parser
;;;; Reads from an input stream and builds DOM nodes directly.
;;;; Designed for well-formed HTML (e.g., browser-sourced output).

(defpackage #:almighty-html/parser/parser
  (:use #:cl)
  (:local-nicknames (#:dom #:almighty-html/parser/dom)
                    (#:is #:almighty-html/parser/input-stream))
  (:export #:parse-html))

(in-package #:almighty-html/parser/parser)

;;; ============================================================
;;; Named character references
;;; ============================================================

(defparameter *named-char-refs*
  '(("amp" . #\&)
    ("lt" . #\<)
    ("gt" . #\>)
    ("quot" . #\")
    ("apos" . #\')
    ("nbsp" . #\Space)
    ("copy" . #\©)
    ("reg" . #\®)
    ("trade" . #\™)
    ("mdash" . #\—)
    ("ndash" . #\–)
    ("laquo" . #\«)
    ("raquo" . #\»)
    ("ldquo" . #\")
    ("rdquo" . #\")
    ("lsquo" . #\')
    ("rsquo" . #\')
    ("hellip" . #\…)
    ("bull" . #\•)
    ("middot" . #\·)
    ("times" . #\×)
    ("divide" . #\÷)
    ("para" . #\¶)
    ("sect" . #\§)
    ("deg" . #\°)
    ("plusmn" . #\±)
    ("frac12" . #\½)
    ("frac14" . #\¼)
    ("frac34" . #\¾))
  "Commonly-used named character references.")

;;; ============================================================
;;; Character reference resolution
;;; ============================================================

(defun resolve-char-ref (stream)
  "Consume a character reference after the initial '&'.
Return the resolved string, or the raw text if not a valid reference."
  (let ((ch (is:stream-peek-char stream)))
    (cond
      ;; Numeric reference: &#
      ((eql ch #\#)
       (is:stream-next-char stream) ; consume #
       (let ((next (is:stream-peek-char stream)))
         (cond
           ;; Hex: &#x or &#X
           ((and next (char-equal next #\x))
            (is:stream-next-char stream) ; consume x
            (resolve-numeric-ref stream 16))
           ;; Decimal: &#digits
           ((and next (digit-char-p next))
            (resolve-numeric-ref stream 10))
           (t "&#"))))
      ;; Named reference: &name;
      ((and ch (alpha-char-p ch))
       (resolve-named-ref stream))
      ;; Bare ampersand
      (t "&"))))

(defun resolve-numeric-ref (stream base)
  "Consume digits for a numeric character reference and return the character as a string."
  (let ((value 0)
        (found nil))
    (loop for ch = (is:stream-peek-char stream)
          while (and ch (digit-char-p ch base))
          do (is:stream-next-char stream)
             (setf value (+ (* value base) (digit-char-p ch base))
                   found t))
    (when (and found (eql (is:stream-peek-char stream) #\;))
      (is:stream-next-char stream)) ; consume ;
    (if (and found (> value 0))
        (string (code-char value))
        (if (= base 16) "&#x" "&#"))))

(defun resolve-named-ref (stream)
  "Consume a named character reference and return the resolved character as a string."
  (let ((name-buf (make-array 10 :element-type 'character :fill-pointer 0 :adjustable t))
        (best-match nil)
        (best-length 0))
    ;; Greedily match the longest known entity name
    (loop for i from 0
          for ch = (is:stream-peek-char stream)
          while (and ch (or (alpha-char-p ch) (digit-char-p ch)))
          do (is:stream-next-char stream)
             (vector-push-extend ch name-buf)
             (let ((ref (assoc (coerce name-buf 'string) *named-char-refs*
                               :test #'string-equal)))
               (when ref
                 (setf best-match ref
                       best-length (1+ i)))))
    (cond
      (best-match
       ;; Put back chars consumed beyond the best match
       (loop repeat (- (length name-buf) best-length)
             do (is:stream-reconsume stream))
       ;; Consume trailing semicolon if present
       (when (eql (is:stream-peek-char stream) #\;)
         (is:stream-next-char stream))
       (string (cdr best-match)))
      (t
       ;; Not a recognized entity — put everything back
       (loop repeat (length name-buf)
             do (is:stream-reconsume stream))
       "&"))))

;;; ============================================================
;;; Text parsing
;;; ============================================================

(defun parse-text (stream)
  "Consume text content until '<' or EOF. Resolves character references.
Returns the text string, or NIL if no text was consumed."
  (let ((buf (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for ch = (is:stream-peek-char stream)
          while (and ch (char/= ch #\<))
          do (is:stream-next-char stream)
             (if (char= ch #\&)
                 (let ((resolved (resolve-char-ref stream)))
                   (loop for c across resolved
                         do (vector-push-extend c buf)))
                 (vector-push-extend ch buf)))
    (when (> (fill-pointer buf) 0)
      (coerce buf 'string))))

(defun parse-raw-text (stream tag-name)
  "Consume raw text content for elements like <script> and <style>.
Reads everything until the matching </tag-name> end tag."
  (let ((buf (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (let ((ch (is:stream-next-char stream)))
        (cond
          ((null ch) (return)) ; EOF
          ((and (char= ch #\<)
                (eql (is:stream-peek-char stream) #\/)
                (check-end-tag stream tag-name))
           ;; We found </tag-name> — consume it and return
           (consume-end-tag stream tag-name)
           (return))
          (t (vector-push-extend ch buf)))))
    (when (> (fill-pointer buf) 0)
      (coerce buf 'string))))

(defun check-end-tag (stream tag-name)
  "Peek ahead to check if we're at </tag-name> (case-insensitive).
Does not consume. The '<' has already been consumed, and we've peeked '/'."
  (let* ((data (is:html-input-stream-data stream))
         (pos (is:html-input-stream-pos stream))
         (len (is:html-input-stream-length stream))
         (tag-len (length tag-name)))
    ;; We need: / tag-name > (or / tag-name whitespace...>)
    ;; pos points at '/'
    (and (<= (+ pos 1 tag-len) len)
         (char= (char data pos) #\/)
         (loop for i from 0 below tag-len
               always (char-equal (char data (+ pos 1 i)) (char tag-name i)))
         (let ((after (+ pos 1 tag-len)))
           (and (< after len)
                (let ((c (char data after)))
                  (or (char= c #\>)
                      (char= c #\Space)
                      (char= c #\Tab)
                      (char= c #\Newline))))))))

(defun consume-end-tag (stream tag-name)
  "Consume the rest of </tag-name>.
The '<' has already been consumed. Consumes '/', tag-name, optional whitespace, and '>'."
  ;; consume /
  (is:stream-next-char stream)
  ;; consume tag name
  (loop repeat (length tag-name) do (is:stream-next-char stream))
  ;; consume whitespace and >
  (loop for ch = (is:stream-peek-char stream)
        while (and ch (char/= ch #\>))
        do (is:stream-next-char stream))
  ;; consume >
  (when (eql (is:stream-peek-char stream) #\>)
    (is:stream-next-char stream)))

;;; ============================================================
;;; Tag and attribute parsing
;;; ============================================================

(defun parse-tag-name (stream)
  "Consume and return a tag name (lowercased). Stream is positioned after '<' or '</'."
  (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for ch = (is:stream-peek-char stream)
          while (and ch
                     (char/= ch #\>)
                     (char/= ch #\/)
                     (char/= ch #\Space)
                     (char/= ch #\Tab)
                     (char/= ch #\Newline))
          do (is:stream-next-char stream)
             (vector-push-extend (char-downcase ch) buf))
    (coerce buf 'string)))

(defun skip-whitespace (stream)
  "Consume and discard whitespace characters."
  (loop for ch = (is:stream-peek-char stream)
        while (and ch (or (char= ch #\Space)
                          (char= ch #\Tab)
                          (char= ch #\Newline)))
        do (is:stream-next-char stream)))

(defun parse-attribute-value (stream)
  "Parse an attribute value: quoted (double or single) or unquoted.
Resolves character references within the value."
  (let ((quote-char (is:stream-peek-char stream)))
    (cond
      ;; Double or single quoted
      ((or (eql quote-char #\") (eql quote-char #\'))
       (is:stream-next-char stream) ; consume opening quote
       (let ((buf (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)))
         (loop for ch = (is:stream-next-char stream)
               while (and ch (char/= ch quote-char))
               do (if (char= ch #\&)
                      (let ((resolved (resolve-char-ref stream)))
                        (loop for c across resolved
                              do (vector-push-extend c buf)))
                      (vector-push-extend ch buf)))
         (coerce buf 'string)))
      ;; Unquoted
      (t
       (let ((buf (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)))
         (loop for ch = (is:stream-peek-char stream)
               while (and ch
                          (char/= ch #\>)
                          (char/= ch #\/)
                          (char/= ch #\Space)
                          (char/= ch #\Tab)
                          (char/= ch #\Newline))
               do (is:stream-next-char stream)
                  (if (char= ch #\&)
                      (let ((resolved (resolve-char-ref stream)))
                        (loop for c across resolved
                              do (vector-push-extend c buf)))
                      (vector-push-extend ch buf)))
         (coerce buf 'string))))))

(defun parse-attributes (stream)
  "Parse attribute pairs until '>' or '/' or EOF.
Returns an alist of (name . value) pairs. Duplicate attributes: first wins."
  (let ((attrs '()))
    (loop
      (skip-whitespace stream)
      (let ((ch (is:stream-peek-char stream)))
        (when (or (null ch) (char= ch #\>) (char= ch #\/))
          (return (nreverse attrs)))
        ;; Parse attribute name
        (let ((name-buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
          (loop for c = (is:stream-peek-char stream)
                while (and c
                           (char/= c #\=)
                           (char/= c #\>)
                           (char/= c #\/)
                           (char/= c #\Space)
                           (char/= c #\Tab)
                           (char/= c #\Newline))
                do (is:stream-next-char stream)
                   (vector-push-extend (char-downcase c) name-buf))
          (let ((name (coerce name-buf 'string)))
            (skip-whitespace stream)
            (let ((value
                    (if (eql (is:stream-peek-char stream) #\=)
                        (progn
                          (is:stream-next-char stream) ; consume =
                          (skip-whitespace stream)
                          (parse-attribute-value stream))
                        "")))
              ;; First attribute wins (per WHATWG)
              (unless (assoc name attrs :test #'string=)
                (push (cons name value) attrs)))))))))

;;; ============================================================
;;; Comment and DOCTYPE parsing
;;; ============================================================

(defun parse-comment (stream)
  "Parse an HTML comment. Stream is positioned after '<!-' and we've confirmed '--'.
Consumes through '-->'."
  ;; Consume the second '-'
  (is:stream-next-char stream)
  (let ((buf (make-array 64 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (let ((ch (is:stream-next-char stream)))
        (cond
          ((null ch) (return)) ; EOF
          ((and (char= ch #\-)
                (eql (is:stream-peek-char stream) #\-))
           ;; Possible end of comment
           (is:stream-next-char stream) ; consume second -
           ;; Consume trailing '>'
           (when (eql (is:stream-peek-char stream) #\>)
             (is:stream-next-char stream))
           (return))
          (t (vector-push-extend ch buf)))))
    (dom:make-comment-node (coerce buf 'string))))

(defun parse-doctype (stream)
  "Parse a DOCTYPE declaration. Stream is positioned after '<!DOCTYPE' or '<!doctype'.
Consumes through '>'."
  (skip-whitespace stream)
  ;; Parse the doctype name
  (let ((name-buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for ch = (is:stream-peek-char stream)
          while (and ch (char/= ch #\>) (char/= ch #\Space)
                     (char/= ch #\Tab) (char/= ch #\Newline))
          do (is:stream-next-char stream)
             (vector-push-extend (char-downcase ch) name-buf))
    ;; Skip the rest until '>'
    (loop for ch = (is:stream-next-char stream)
          while (and ch (char/= ch #\>)))
    (dom:make-doctype-node :name (coerce name-buf 'string))))

(defun parse-markup-declaration (stream)
  "Parse after '<!'. Dispatches to comment or DOCTYPE.
Stream is positioned after '<!'. Returns a DOM node or NIL."
  (let ((ch (is:stream-peek-char stream)))
    (cond
      ;; Comment: <!--
      ((and (eql ch #\-)
            (let* ((data (is:html-input-stream-data stream))
                   (pos (is:html-input-stream-pos stream))
                   (len (is:html-input-stream-length stream)))
              (and (< pos len) (char= (char data pos) #\-))))
       (is:stream-next-char stream) ; consume first -
       (parse-comment stream))
      ;; DOCTYPE: <!DOCTYPE
      ((and ch (char-equal ch #\D)
            (is:stream-match-insensitive stream "DOCTYPE"))
       (is:stream-consume-chars stream 7) ; consume "DOCTYPE"
       (parse-doctype stream))
      ;; Bogus: treat as comment (e.g., <![CDATA[ etc.)
      (t
       (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
         (loop for c = (is:stream-next-char stream)
               while (and c (char/= c #\>))
               do (vector-push-extend c buf))
         (dom:make-comment-node (coerce buf 'string)))))))

;;; ============================================================
;;; Element parsing
;;; ============================================================

(defun parse-element (stream)
  "Parse an element starting after '<'. Reads tag name, attributes,
handles void elements, raw text elements, and recurses into children."
  (let* ((tag-name (parse-tag-name stream))
         (attrs (parse-attributes stream))
         (self-closing (eql (is:stream-peek-char stream) #\/)))
    ;; Consume / if self-closing
    (when self-closing
      (is:stream-next-char stream))
    ;; Consume >
    (when (eql (is:stream-peek-char stream) #\>)
      (is:stream-next-char stream))
    (let ((element (dom:make-element tag-name :attributes attrs)))
      (cond
        ;; Void element — no children, no end tag
        ((or self-closing
             (member tag-name dom:*void-elements* :test #'string-equal))
         element)
        ;; Raw text element — consume raw text until </tag>
        ((member tag-name dom:*raw-text-elements* :test #'string-equal)
         (let ((text (parse-raw-text stream tag-name)))
           (when text
             (dom:append-child element (dom:make-text-node text))))
         element)
        ;; Normal element — recurse into children
        (t
         (parse-children stream element tag-name)
         element)))))

(defun parse-children (stream parent parent-tag)
  "Parse children of PARENT element until we hit </parent-tag> or EOF."
  (loop
    (let ((ch (is:stream-peek-char stream)))
      (cond
        ;; EOF — done
        ((null ch) (return))
        ;; Possible tag
        ((char= ch #\<)
         (is:stream-next-char stream) ; consume <
         (let ((next (is:stream-peek-char stream)))
           (cond
             ;; End tag: </
             ((eql next #\/)
              (is:stream-next-char stream) ; consume /
              (let ((end-name (parse-tag-name stream)))
                ;; Skip whitespace and consume >
                (skip-whitespace stream)
                (when (eql (is:stream-peek-char stream) #\>)
                  (is:stream-next-char stream))
                ;; If this matches our parent, we're done
                (when (string-equal end-name parent-tag)
                  (return))
                ;; Otherwise ignore the unmatched end tag
                ))
             ;; Comment / DOCTYPE: <!
             ((eql next #\!)
              (is:stream-next-char stream) ; consume !
              (let ((node (parse-markup-declaration stream)))
                (when node
                  (dom:append-child parent node))))
             ;; Bogus: <? (processing instruction — treat as comment)
             ((eql next #\?)
              (is:stream-next-char stream)
              (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
                (loop for c = (is:stream-next-char stream)
                      while (and c (char/= c #\>))
                      do (vector-push-extend c buf))
                (dom:append-child parent
                                  (dom:make-comment-node
                                   (concatenate 'string "?" (coerce buf 'string))))))
             ;; Start tag
             ((and next (alpha-char-p next))
              (let ((child (parse-element stream)))
                (dom:append-child parent child)))
             ;; Lone '<' not followed by tag-like char — treat as text
             (t
              (let ((text-buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
                (vector-push-extend #\< text-buf)
                ;; Continue consuming text
                (loop for tc = (is:stream-peek-char stream)
                      while (and tc (char/= tc #\<))
                      do (is:stream-next-char stream)
                         (if (char= tc #\&)
                             (let ((resolved (resolve-char-ref stream)))
                               (loop for c across resolved
                                     do (vector-push-extend c text-buf)))
                             (vector-push-extend tc text-buf)))
                (merge-text parent (coerce text-buf 'string)))))))
        ;; Text content
        (t
         (let ((text (parse-text stream)))
           (when text
             (merge-text parent text))))))))

(defun merge-text (parent text)
  "Append text to PARENT, merging with the last child if it's a text node."
  (let ((last (dom:last-child parent)))
    (if (typep last 'dom:text-node)
        (setf (dom:text-data last)
              (concatenate 'string (dom:text-data last) text))
        (dom:append-child parent (dom:make-text-node text)))))

;;; ============================================================
;;; Top-level parse functions
;;; ============================================================

(defun parse-nodes (stream parent)
  "Parse top-level nodes (text, elements, comments, doctype) into PARENT."
  (loop
    (let ((ch (is:stream-peek-char stream)))
      (cond
        ((null ch) (return))
        ((char= ch #\<)
         (is:stream-next-char stream) ; consume <
         (let ((next (is:stream-peek-char stream)))
           (cond
             ;; End tag at top level — ignore
             ((eql next #\/)
              (is:stream-next-char stream)
              (let ((_name (parse-tag-name stream)))
                (declare (ignore _name))
                (skip-whitespace stream)
                (when (eql (is:stream-peek-char stream) #\>)
                  (is:stream-next-char stream))))
             ;; Comment / DOCTYPE
             ((eql next #\!)
              (is:stream-next-char stream)
              (let ((node (parse-markup-declaration stream)))
                (when node
                  (dom:append-child parent node))))
             ;; Processing instruction
             ((eql next #\?)
              (is:stream-next-char stream)
              (let ((buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
                (loop for c = (is:stream-next-char stream)
                      while (and c (char/= c #\>))
                      do (vector-push-extend c buf))
                (dom:append-child parent
                                  (dom:make-comment-node
                                   (concatenate 'string "?" (coerce buf 'string))))))
             ;; Start tag
             ((and next (alpha-char-p next))
              (let ((child (parse-element stream)))
                (dom:append-child parent child)))
             ;; Lone <
             (t
              (let ((text-buf (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)))
                (vector-push-extend #\< text-buf)
                (loop for tc = (is:stream-peek-char stream)
                      while (and tc (char/= tc #\<))
                      do (is:stream-next-char stream)
                         (if (char= tc #\&)
                             (let ((resolved (resolve-char-ref stream)))
                               (loop for c across resolved
                                     do (vector-push-extend c text-buf)))
                             (vector-push-extend tc text-buf)))
                (merge-text parent (coerce text-buf 'string)))))))
        (t
         (let ((text (parse-text stream)))
           (when text
             (merge-text parent text))))))))

(defun parse-html (input)
  "Parse an HTML string and return a document node."
  (let ((stream (is:make-html-input-stream input))
        (doc (dom:make-document)))
    (parse-nodes stream doc)
    doc))
