(defpackage #:almighty-html
  (:nicknames #:almighty-html/main)
  (:use #:cl
    #:almighty-html/element
    #:almighty-html/dsl
    #:almighty-html/utils
    #:almighty-html/converter)
  (:import-from #:almighty-html/builtin)
  (:import-from #:almighty-html/web-components)
  (:export #:</>
           #:define-component
           #:register-web-components
           #:clear-web-components
           #:render-to-string
           #:clsx
           #:convert-html-to-almighty))
(in-package :almighty-html)
