;;;; package.lisp — Re-export facade for almighty-html/parser
;;;; Provides the public API by re-exporting symbols from sub-packages.

(defpackage #:almighty-html/parser
  (:use #:cl)

  ;; Import from almighty-html/parser/dom
  (:import-from #:almighty-html/parser/dom
                #:node #:document-node #:element-node #:text-node #:comment-node #:doctype-node
                #:node-parent #:node-children #:node-type
                #:document-doctype #:document-quirks-mode #:make-document #:document-children
                #:element-tag-name #:element-namespace #:element-attributes #:element-attribute
                #:make-element
                #:text-data #:make-text-node
                #:comment-data #:make-comment-node
                #:doctype-name #:doctype-public-id #:doctype-system-id #:make-doctype-node
                #:append-child #:insert-child-before #:remove-child #:last-child
                #:child-elements #:child-elements-by-tag #:first-child-element
                #:get-element-by-id #:get-elements-by-tag-name #:inner-text)

  ;; Import from almighty-html/parser/serializer
  (:import-from #:almighty-html/parser/serializer
                #:serialize-node
                #:lispify-node
                #:write-lispified-node)

  ;; Import from almighty-html/parser/parser
  (:import-from #:almighty-html/parser/parser
                #:parse-html)

  ;; Re-export the public API
  (:export
   ;; Main API
   #:parse-html

   ;; DOM node types
   #:node
   #:document-node
   #:element-node
   #:text-node
   #:comment-node
   #:doctype-node

   ;; DOM accessors
   #:node-parent
   #:node-children
   #:element-tag-name
   #:element-namespace
   #:element-attributes
   #:element-attribute
   #:text-data
   #:comment-data
   #:doctype-name
   #:doctype-public-id
   #:doctype-system-id
   #:document-children

   ;; DOM utilities
   #:node-type
   #:child-elements
   #:child-elements-by-tag
   #:first-child-element
   #:get-element-by-id
   #:get-elements-by-tag-name
   #:inner-text
   #:serialize-node
   #:lispify-node
   #:write-lispified-node))
