(defpackage #:almighty-html/builtin
  (:use #:cl)
  (:import-from #:almighty-html/dsl
                #:deftag))
(in-package #:almighty-html/builtin)

(defmacro register-builtin-tags (&rest names)
  `(progn
     ,@(mapcan (lambda (name)
                 (list `(deftag ,name)
                       `(export ',name)))
               names)))

(register-builtin-tags
 ;; Standard HTML elements (comprehensive, modern HTML5+)
 a abbr address area article aside audio b base bdi bdo blockquote body br button canvas caption cite code col colgroup data datalist
 dd del details dfn dialog div dl dt em embed fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 hgroup head header html hr i iframe
 img input ins kbd label legend li link main |map| mark math menu meta meter nav noscript object ol optgroup option output p param picture pre progress
 q rb rtc rp rt ruby s samp script section select slot small source span strong style sub summary sup table tbody td template textarea tfoot th thead |time| title tr track u ul var video wbr

 ;; All standard SVG elements (SVG 1.1 + SVG 2, non-deprecated)
 ;; Structural / grouping
 svg g defs |symbol| use desc metadata

 ;; Basic shapes
 rect circle ellipse line polyline polygon path mesh

 ;; Text
 text tspan textpath

 ;; Containers / other graphics
 image foreignobject switch view

 ;; Clipping, masking, markers
 clippath mask marker

 ;; Gradients, patterns, paint servers (SVG 1.1 + SVG 2)
 lineargradient radialgradient pattern stop solidcolor hatch hatchpath meshgradient

 ;; Filters and filter primitives
 filter
 feblend fecolormatrix fecomponenttransfer fecomposite feconvolvematrix
 fediffuselighting fedisplacementmap fedropshadow feflood fefunca fefuncb fefuncg fefuncr
 fegaussianblur feimage femerge femergenode femorphology feoffset fespecularlighting fetile feturbulence
 fedistantlight fepointlight fespotlight

 ;; Animation
 animate animatemotion animatetransform |set| discard mpath

 ;; Miscellaneous
 cursor

                                        ; special tags
 <> raw!)
