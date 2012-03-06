;; This software is Copyright (c) cage
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package cl-pslib)

(defmacro define-string-constant ((prefix) &rest names)
  `(progn
     ,@(mapcar #'(lambda (cn) 
		   `(alexandria:define-constant
			,(alexandria:format-symbol t "+~:@(~a-~a~)+" prefix cn)
			,cn :test #'string=))
	       names)))


(alexandria:define-constant +true+ "true" :test #'string=)
(alexandria:define-constant +false+ "false" :test #'string=)


(define-string-constant ("local-link") "fitpage" "fitwidth" "fitheight" "fitbbox")
(define-string-constant ("pdf-link") "fitpage" "fitwidth" "fitheight" "fitbbox")

(alexandria:define-constant +pattern-paint-type1+ 1 :test #'=)
(alexandria:define-constant +pattern-paint-type2+ 2 :test #'=)


(define-string-constant ("find-font-encoding") "builtin")


(define-string-constant ("value-key") "fontsize" "font" "imagewidth" "imageheight"
			"capheight" "ascender" "descender"
			"italicangle" "underlineposition" "underlinethickness"
			"textx" "texty" "textrendering" "wordspacing"
			"major" "minor" "subminor" "revision"
			"charspacing" "hyphenminchars" "separationcolor"
			"textrise" "leading" "hyphenminchars"
			"parindent" "numindentlines" "parskip"
			"parindentskip" "linenumberspace" "linenumbersep"
			"boxheight")

(define-string-constant ("parameter-key") "FontAFM" "FontOutline" "FontProtusion"
			"FontEncoding" "RightMarginKerning" "LeftMarginKerning"
			"RightMarginKerning" "LeftMarginKerning" "SearchPath"
			"underline" "overline" "strikeout" "warning" "hyphendict"
			"inputencoding" "linenumbermode" "ligaturedisolvechar"
			"imagereuse" "imageencoding")

    

(define-string-constant ("image-file") "mask" "masked")
(define-string-constant ("image-file-type") "png" "jpeg" "gif" "tiff" "bmp" "eps")

(define-string-constant ("image-type") "raw" "eps")

(define-string-constant ("shading") "radial" "axial")

(define-string-constant ("shading-opt") "extend0" "extend0" "r0" "r1")

(alexandria:define-constant +ps-linecap-butt+ PS_LINECAP_BUTT :test #'=)
(alexandria:define-constant +ps-linecap-round+ PS_LINECAP_ROUND :test #'=)

(alexandria:define-constant +ps_linejoin_miter+ PS_LINEJOIN_MITER :test #'=)
(alexandria:define-constant +ps_linejoin_round+ PS_LINEJOIN_ROUND :test #'=)
(alexandria:define-constant +ps_linejoin_bevel+ PS_LINEJOIN_BEVEL :test #'=)

(define-string-constant ("link-border-style") "solid" "dashed")

(define-string-constant ("ps-comment-key") "Keywords" "Subject" "Title" 
			"Creator" "Author" "BoundingBox" "Orientation")

(define-string-constant ("color-type") "fill" "stroke" "fillstroke")

(define-string-constant ("color-space") "gray" "spot" "rgb" "cmyk" "pattern")

(define-string-constant ("line-number-mode") "box" "paragraph")

(define-string-constant ("boxed-text-feature") "blind")

(define-string-constant ("boxed-text-h-mode") "justify" "fulljustify" "right" "left" "center")