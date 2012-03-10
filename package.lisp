;; This software is Copyright (c) cage
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL


(defpackage #:cl-pslib
  (:use :cl :cffi :cl-colors)
  (:nicknames :ps)
  (:export
   :page-size
   :width
   :height
   :+a0-paper-width+ 
   :+a0-paper-height+ 
   :+a1-paper-width+ 
   :+a1-paper-height+ 
   :+a2-paper-width+ 
   :+a2-paper-height+ 
   :+a3-paper-width+ 
   :+a3-paper-height+ 
   :+a4-paper-width+ 
   :+a4-paper-height+ 
   :+a5-paper-width+ 
   :+a5-paper-height+ 
   :+a6-paper-width+ 
   :+a6-paper-height+ 
   :+a7-paper-width+ 
   :+a7-paper-height+ 
   :+a8-paper-width+ 
   :+a8-paper-height+ 
   :+a9-paper-width+ 
   :+a9-paper-height+ 
   :+a10-paper-width+ 
   :+a10-paper-height+
   :+b0-paper-width+ 
   :+b0-paper-height+ 
   :+b1-paper-width+ 
   :+b1-paper-height+ 
   :+b2-paper-width+ 
   :+b2-paper-height+ 
   :+b3-paper-width+ 
   :+b3-paper-height+ 
   :+b4-paper-width+ 
   :+b4-paper-height+ 
   :+b5-paper-width+ 
   :+b5-paper-height+ 
   :+b6-paper-width+ 
   :+b6-paper-height+ 
   :+b7-paper-width+ 
   :+b7-paper-height+ 
   :+b8-paper-width+ 
   :+b8-paper-height+ 
   :+b9-paper-width+ 
   :+b9-paper-height+ 
   :+b10-paper-width+ 
   :+b10-paper-height+
   :+c0-paper-width+ 
   :+c0-paper-height+ 
   :+c1-paper-width+ 
   :+c1-paper-height+ 
   :+c2-paper-width+ 
   :+c2-paper-height+ 
   :+c3-paper-width+ 
   :+c3-paper-height+ 
   :+c4-paper-width+ 
   :+c4-paper-height+ 
   :+c5-paper-width+ 
   :+c5-paper-height+ 
   :+c6-paper-width+ 
   :+c6-paper-height+ 
   :+c7-paper-width+ 
   :+c7-paper-height+ 
   :+c8-paper-width+ 
   :+c8-paper-height+ 
   :+c9-paper-width+ 
   :+c9-paper-height+ 
   :+c10-paper-width+ 
   :+c10-paper-height+

   :+a0-page-size+
   :+a1-page-size+
   :+a2-page-size+
   :+a3-page-size+
   :+a4-page-size+
   :+a5-page-size+
   :+a6-page-size+
   :+a7-page-size+
   :+a8-page-size+
   :+a9-page-size+
   :+a10-page-size+


   :millimiter->centimeter
   :millimiter->inch
   :millimiter->point 
   :centimeter->millimeter
   :centimeter->inch
   :centimeter->point
   :inch->millimeter
   :inch->centimeter
   :inch->point
   :point->millimeter
   :point->centimeter
   :point->inch

   :aabb->rect 
   :rect->aabb 
   :inside-aabb-p 
   :line-eqn
   :recursive-bezier 
   :2d-vector-map 
   :2d-vector-list-map 
   :2d-vector-list-scale 
   :2d-vector-list-translate 
   :2d-vector-list-rotate 
   :2d-vector-sum 
   :2d-vector-diff 
   :2d-vector-dot-product 
   :2d-vector-cross-product 
   :2d-vector-scale 
   :2d-vector-translate 
   :2d-vector-magn 
   :2d-vector-normalize 
   :2d-vector-angle 
   :2d-vector-rotate 
   :xy->pair 
   :pair->interleaved-xy 
   :xy->interleaved-xy 
   :interleaved-xy->pair 


   :pslib_errornum<0
   :*callback-string*
   :write-to-string

   :text-error
   :spotcolor-error
   :bookmark-error
   :image-load-error
   :shading-pattern-error
   :not-implemented-error
   :null-reference
   :out-of-bounds

   :+true+
   :+false+
   :+local-link-fitpage+ 
   :+local-link-fitwidth+ 
   :+local-link-fitheight+
   :+local-link-fitbbox+ 
   :+pdf-link-fitpage+ 
   :+pdf-link-fitwidth+ 
   :+pdf-link-fitheight+
   :+pdf-link-fitbbox+ 
   :+pattern-paint-type1+
   :+pattern-paint-type2+
   :+find-font-encoding-builtin+
   :+value-key-fontsize+
   :+value-key-font+
   :+value-key-imagewidth+
   :+value-key-imageheight+
   :+value-key-capheight+
   :+value-key-ascender+
   :+value-key-descender+
   :+value-key-italicangle+
   :+value-key-underlineposition+
   :+value-key-underlinethickness+
   :+value-key-textx+
   :+value-key-texty+
   :+value-key-textrendering+
   :+value-key-wordspacing+
   :+value-key-major+
   :+value-key-minor+
   :+value-key-subminor+
   :+value-key-revision+
   :+value-key-charspacing+
   :+value-key-hyphenminchars+
   :+value-key-separationcolor+
   :+value-key-textrise+
   :+value-key-leading+
   :+value-key-hyphenminchars+
   :+value-key-parindent+
   :+value-key-numindentlines+
   :+value-key-parskip+
   :+value-key-parindentskip+
   :+value-key-linenumberspace+
   :+value-key-linenumbersep+
   :+value-key-boxheight+
   :+parameter-key-fontafm+
   :+parameter-key-fontoutline+
   :+parameter-key-fontprotusion+
   :+parameter-key-fontencoding+
   :+parameter-key-rightmarginkerning+
   :+parameter-key-leftmarginkerning+
   :+parameter-key-rightmarginkerning+
   :+parameter-key-leftmarginkerning+
   :+parameter-key-searchpath+
   :+parameter-key-underline+
   :+parameter-key-overline+
   :+parameter-key-strikeout+
   :+parameter-key-warning+
   :+parameter-key-hyphendict+
   :+parameter-key-inputencoding+
   :+parameter-key-linenumbermode+
   :+parameter-key-ligaturedisolvechar+
   :+parameter-key-imagereuse+
   :+parameter-key-imageencoding+
   :+image-file-mask+
   :+image-file-masked+
   :+image-file-type-png+
   :+image-file-type-jpeg+
   :+image-file-type-gif+
   :+image-file-type-tiff+
   :+image-file-type-bmp+
   :+image-file-type-eps+
   :+image-type-raw+
   :+image-type-eps+
   :+shading-radial+
   :+shading-axial+
   :+shading-opt-extend0+
   :+shading-opt-extend0+
   :+shading-opt-r0+
   :+shading-opt-r1+
   :+ps-linecap-butt+
   :+ps-linecap-round+
   :+link-border-style-solid+
   :+link-border-style-dashed+
   :+ps-comment-key-keywords+
   :+ps-comment-key-subject+
   :+ps-comment-key-title+
   :+ps-comment-key-creator+
   :+ps-comment-key-author+
   :+ps-comment-key-boundingbox+
   :+ps-comment-key-orientation+
   :+color-type-fill+
   :+color-type-stroke+
   :+color-type-fillstroke+
   :+color-space-gray+
   :+color-space-spot+
   :+color-space-rgb+
   :+color-space-cmyk+
   :+color-space-pattern+
   :+line-number-mode-box+
   :+line-number-mode-paragraph+
   :+boxed-text-feature-blind+
   :+boxed-text-h-mode-justify+
   :+boxed-text-h-mode-fulljustify+
   :+boxed-text-h-mode-right+
   :+boxed-text-h-mode-left+
   :+boxed-text-h-mode-center+
   :*conversion-metrics*
   :open-doc
   :close-doc
   :shutdown
   :begin-page
   :end-page
   :moveto
   :closepath
   :lineto
   :rect
   :circle
   :arc
   :arcn
   :curveto
   :stroke
   :fill-path
   :fill-stroke
   :add-bookmark
   :add-kerning
   :add-launchlink
   :add-ligature
   :add-locallink
   :add-note
   :add-pdflink
   :add-weblink
   :begin-font
   :begin-glyph
   :begin-pattern
   :begin-template
   :clip
   :close-image
   :close-path
   :close-path-stroke
   :continue-text
   :continue-text2
   :create-gstate
   :delete
   :end-font
   :end-glyph
   :end-page
   :end-pattern
   :end-template
   :findfont
   :get-majorversion
   :get-minorversion
   :get-parameter
   :get-value
   :glyph-show
   :include-file 
   :lineto
   :moveto
   :makespotcolor
   :open-image-file
   :open-image
   :place-image
   :restore
   :rotate
   :save
   :scale
   :set-border-link-color
   :set-border-link-dash
   :set-border-link-style
   :set-info
   :set-parameter
   :set-text-pos
   :set-value
   :setcolor
   :shading
   :setpolydash
   :setflat
   :setgray
   :setlinecap
   :setlinejoin
   :setlinewidth
   :setmiterlimit
   :shading-pattern
   :shfill
   :show
   :show-xy
   :string-geometry
   :font-symbol
   :font-symbol-name
   :font-symbol-width
   :translate
   :curve-to
   :bezier-to))

   