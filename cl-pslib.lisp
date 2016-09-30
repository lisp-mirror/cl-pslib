;; This software is Copyright (c) cage
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package #:cl-pslib)

(defparameter *is-ps-boot-called* nil)

(defparameter *conversion-metrics* #'millimiter->point)

(defmacro co-sf (val)
  `(coerce ,val 'single-float))

(defmacro conv-mt (val)
  `(funcall *conversion-metrics* (co-sf ,val)))

(defclass psdoc ()
  ((psdoc-pointer
    :initform nil
    :accessor psdoc-pointer
    :initarg :psdoc-pointer)
   (page-size
    :initform (make-instance 'page-size)
    :accessor page-size
    :initarg :page-size
    :type page-size)
   (filename
    :initform nil
    :accessor filename
    :type string)
   (writeproc
    :initform (callback write-to-string)
    :accessor writeproc
    :initarg :writeproc
    :type cffi:callback)))

(defmethod print-object ((object psdoc) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "pointer ~s~%sizes: ~a~%" (psdoc-pointer object) (page-size object))))

(defmethod initialize-instance :after ((object psdoc) &key)
  (when (not *is-ps-boot-called*)
    (ps_boot))
  (setf (psdoc-pointer  object) (ps_new)))

(defgeneric begin-page (object))

(defgeneric end-page (object))

(defgeneric curveto (object x1 y1 x2 y2 x3 y3))

(defgeneric add-bookmark (object text parent open))

(defgeneric add-kerning (object font-id glyph-name1 glyph-name2 kern))

(defgeneric add-launchlink (object llx lly urx ury file-name))

(defgeneric add-ligature (object font-id glyph-name1 glyph-name2 glyph-name3))

(defgeneric add-locallink (object llx lly urx ury page dest))

(defgeneric add-note (object llx lly urx ury contents title icon open))

(defgeneric add-pdflink (object llx lly urx ury filename page dest))

(defgeneric add-weblink (object llx lly urx ury url))

(defgeneric arc (object x y radius alpha beta))

(defgeneric arcn (object x y radius alpha beta))

(defgeneric begin-font (object font-name reserved a b c d e f opt-list))

(defgeneric begin-glyph (object glyph-name wx llx lly urx ury))

(defgeneric begin-pattern (object width height xstep ystep paint-type))

(defgeneric begin-template (object width height))

(defgeneric circle (object x y radius))

(defgeneric clip (object))

(defgeneric close-doc (object))

(defgeneric close-image (object image-id))

(defgeneric closepath (object))

(defgeneric closepath-stroke (object))

(defgeneric continue-text (object text))

(defgeneric continue-text2 (object text length))

(defgeneric create-gstate (object opt-list))

(defgeneric delete-doc (object))

(defgeneric end-font (object))

(defgeneric end-glyph (object))

(defgeneric end-page (object))

(defgeneric end-pattern (object))

(defgeneric end-template (object))

(defgeneric fill-path (object))

(defgeneric fill-stroke (object))

(defgeneric findfont (object font-name encoding embed))

(defgeneric get-parameter (object name &optional modifier))

(defgeneric get-value (object name &optional modifier))

(defgeneric glyph-show (object glyph-name))

(defgeneric include-file (object ps-filename))

(defgeneric lineto (object x y))

(defgeneric makespotcolor (object name &optional reserved))

(defgeneric moveto (object x y))

(defgeneric open-doc (object path))

(defgeneric open-image-file (object type file-name param int-param))

(defgeneric open-image (object type source data length width height components bpc params))

(defgeneric place-image (object image-id x y scale))

(defgeneric rect (object x y width height))

(defgeneric restore (object))

(defgeneric rotate (object rot))

(defgeneric save (object))

(defgeneric scale (object scale-factor-x scale-factor-y))

(defgeneric stroke (object))

(defgeneric set-border-link-color (object r g b))

(defgeneric set-border-link-dash (object black white))

(defgeneric set-border-link-style (object style width))

(defgeneric setflat (object val))

(defgeneric setgray (object level))

(defgeneric set-info (psdoc key val))

(defgeneric set-parameter (psdoc key val))

(defgeneric set-text-pos (object x y))

(defgeneric set-value (psdoc key val))

(defgeneric setcolor (object type  color-space &optional c1 c2 c3 c4))

(defgeneric setfont (object font-id size))

(defgeneric setlinecap (object type))

(defgeneric setlinejoin (object type))

(defgeneric setlinewidth (object type))

(defgeneric setmiterlimit (object type))

(defgeneric setpolydash (object black-white))

(defgeneric shading (object shading-type x0 y0 x1 y1 c1 c2 c3 c4 options))

(defgeneric shading-pattern (object shading-id &optional option-list))

(defgeneric shfill (object shading-id))

(defgeneric show (object text &optional x-len))

(defgeneric show-boxed (object text left top width height h-mode feature))

(defgeneric show-xy (object text x y &optional x-len))

(defgeneric string-geometry (object text size font-id &key end))

(defgeneric font-symbol (object char))

(defgeneric font-symbol-name (object idx name &optional font-id size))

(defgeneric font-symbol-width (object idx  &optional font-id size))

(defgeneric translate (object x y))

(defgeneric curve-to (object p1 p2 p3))

(defgeneric bezier-to (object p1 p2 p3 p4 &key threshold))

(defun shutdown ()
  (ps_shutdown))

(defun get-majorversion ()
  (ps_get_majorversion))

(defun get-minorversion ()
  (ps_get_minorversion))

(defmacro define-only-psdoc-method (lispname)
  `(progn
     ,@(mapcar #'(lambda (name)
		   `(defmethod ,(alexandria:format-symbol t "~@:(~a~)" name) ((object psdoc))
		      (with-psdoc-ptr (ptr) object
			(,(alexandria:format-symbol t "PS_~a" (cl-ppcre:regex-replace-all "-" (symbol-name name) "_")) ptr))))
	       lispname)))

(defmacro with-psdoc-ptr ((ptr) object &body body)
  `(with-accessors ((,ptr psdoc-pointer)) ,object
     ,@body))

(define-only-psdoc-method (end-font end-glyph end-page end-pattern end-template restore save))

(defmethod open-doc ((object psdoc) (file pathname))
  (pslib_errornum<0 (open-doc object (namestring file))))

(defmethod open-doc ((object psdoc) (file string))
  (with-accessors ((filename filename)
		   (ptr psdoc-pointer)) object
    (setf filename file)
    (pslib_errornum<0 (ps_open_file ptr file))))

(defmethod open-doc ((object psdoc) (file (eql nil))) ; open doc in memory
  (with-accessors ((ptr psdoc-pointer)
		   (writeproc writeproc)) object
    (pslib_errornum<0 (ps_open_mem ptr writeproc))))

(defmethod close-doc ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (ps_close ptr)))

(defmethod begin-page ((object psdoc))
  (with-accessors ((ptr psdoc-pointer)
		   (page-size page-size)) object
    (ps_begin_page ptr
		   (millimiter->point (width page-size))
		   (millimiter->point (height page-size)))))

(defmethod end-page ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (ps_end_page ptr)))

(defmethod moveto ((object psdoc) (x number) (y number))
  (with-psdoc-ptr (ptr) object
    (ps_moveto ptr (conv-mt x) (conv-mt y))))

(defmethod closepath ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (ps_closepath ptr)))

(defmethod lineto ((object psdoc) (x number) (y number))
  (with-psdoc-ptr (ptr) object
    (ps_lineto ptr (conv-mt x) (conv-mt y))))

(defmethod rect ((object psdoc) (x number) (y number) (width number) (height number))
  (with-psdoc-ptr (ptr) object
    (ps_rect ptr (conv-mt x) (conv-mt y) (conv-mt width) (conv-mt height))))

(defmethod circle ((object psdoc) (x number) (y number) (radius number))
  (with-psdoc-ptr (ptr) object
    (ps_circle ptr (conv-mt x) (conv-mt y) (conv-mt radius))))

(defmethod arc ((object psdoc) (x number) (y number) (radius number) (alpha number) (beta number))
  (with-psdoc-ptr (ptr) object
    (ps_arc ptr (conv-mt x) (conv-mt y) (conv-mt radius) (co-sf alpha) (co-sf beta))))

(defmethod arcn ((object psdoc) (x number) (y number) (radius number) (alpha number) (beta number))
  (with-psdoc-ptr (ptr) object
    (ps_arcn ptr (conv-mt x) (conv-mt y) (conv-mt radius) (co-sf alpha) (co-sf beta))))

(defmethod curveto ((object psdoc) (x1 number) (y1 number) (x2 number) (y2 number) (x3 number) (y3 number))
  (with-psdoc-ptr (ptr) object
    (ps_curveto ptr (conv-mt x1) (conv-mt y1) (conv-mt x2) (conv-mt y2) (conv-mt x3) (conv-mt y3))))

(defmethod stroke ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (ps_stroke ptr)))

(defmethod fill-path ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (ps_fill ptr)))

(defmethod fill-stroke ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (ps_fill_stroke ptr)))

(defmethod add-bookmark ((object psdoc) (text string) (parent integer) open)
  (with-psdoc-ptr (ptr) object
    (let ((bookmark-id (ps_add_bookmark ptr text parent (truth-lisp->c open))))
      (if (<= bookmark-id 0)
	  (error 'bookmark-error
		 :text (format nil "Can not set bookmark for ~a (parent ~a)"
			       text parent))
	  bookmark-id))))

(defmethod add-kerning ((object psdoc) (font-id integer)
			(glyph-name1 string) (glyph-name2 string) (kern integer))
  (with-psdoc-ptr (ptr) object
    (ps_add_kerning ptr font-id glyph-name1 glyph-name2 kern)))

(defmethod add-launchlink ((object psdoc)
			    (llx number) (lly number)
			    (urx number) (ury number) (file-name string))
  (with-psdoc-ptr (ptr) object
    (ps_add_launchlink ptr (conv-mt llx) (conv-mt lly)
		       (conv-mt urx) (conv-mt ury) file-name)))

(defmethod add-ligature ((object psdoc) (font-id integer)
			 (glyph-name1 string) (glyph-name2 string) (glyph-name3 string))
  (with-psdoc-ptr (ptr) object
    (ps_add_ligature ptr font-id glyph-name1 glyph-name2 glyph-name3)))

(defmethod add-locallink ((object psdoc)
			   (llx number) (lly number)
			   (urx number) (ury number)
			   (page integer) (dest string))
  (with-psdoc-ptr (ptr) object
    (ps_add_locallink ptr
		      (conv-mt llx) (conv-mt lly)
		      (conv-mt urx) (conv-mt ury) page dest)))

(defmethod add-note ((object psdoc)
		     (llx number) (lly number)
		     (urx number) (ury number)
		     (contents string)
		     (title string)
		     (icon string) open)
  (with-psdoc-ptr (ptr) object
    (ps_add_note ptr
		 (conv-mt llx) (conv-mt lly)
		 (conv-mt urx) (conv-mt ury)
		 contents title icon
		 (truth-lisp->c open))))

(defmethod add-pdflink ((object psdoc)
			(llx number) (lly number)
			(urx number) (ury number)
			(file-name string)
			(page integer) (dest string))
  (with-psdoc-ptr (ptr) object
    (ps_add_pdflink ptr
		    (conv-mt llx) (conv-mt lly)
		    (conv-mt urx) (conv-mt ury)
		    file-name page dest)))

(defmethod add-weblink ((object psdoc)
			 (llx number) (lly number)
			 (urx number) (ury number)
			 (url string))
  (with-psdoc-ptr (ptr) object
    (ps_add_weblink ptr
		    (conv-mt llx) (conv-mt lly)
		    (conv-mt urx) (conv-mt ury)
		    url)))

(defmethod begin-font ((object psdoc)
			(font-name string)
			(reserved integer)
			(a number)
			(b number)
			(c number)
			(d number)
			(e number)
			(f number) (opt-list string))
  (with-psdoc-ptr (ptr) object
    (ps_begin_font ptr font-name reserved
		   (co-sf a)
		   (co-sf b) (co-sf c)
		   (co-sf d) (co-sf e) (co-sf f) opt-list)))

(defmethod begin-glyph ((object psdoc)
			(glyph-name string)
			(wx number)
			(llx number)
			(lly number)
			(urx number)
			(ury number))
  (with-psdoc-ptr (ptr) object
    (ps_begin_glyph ptr glyph-name (conv-mt wx)
		    (conv-mt llx)
		    (conv-mt lly)
		    (conv-mt urx)
		    (conv-mt ury))))

(defmethod begin-pattern ((object psdoc)
			  (width number)
			  (height number)
			  (xstep number)
			  (ystep number)
			  (paint-type integer))
  (with-psdoc-ptr (ptr) object
    (pslib_errornum<0 (ps_begin_pattern ptr
					(conv-mt width)
					(conv-mt height)
					(conv-mt xstep)
					(conv-mt ystep)
					paint-type))))

(defmethod begin-template ((object psdoc) (width number) (height number))
  (with-psdoc-ptr (ptr) object
    (pslib_errornum<0
     (ps_begin_template ptr (conv-mt width) (conv-mt height)))))

(defmethod clip ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (pslib_errornum<0
     (ps_clip ptr))))

(defmethod close-image ((object psdoc) (image-id integer))
  (with-psdoc-ptr (ptr) object
    (ps_close_image ptr image-id)))

(defmethod closepath ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (ps_closepath ptr)))

(defmethod closepath-stroke ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (ps_closepath_stroke ptr)))

(defmethod continue-text ((object psdoc) (text string))
  (with-psdoc-ptr (ptr) object
    (ps_continue_text ptr text)))

(defmethod continue-text2 ((object psdoc) (text string) (length integer))
  (with-psdoc-ptr (ptr) object
    (ps_continue_text2 ptr text length)))

(defmethod create-gstate ((object psdoc) (opt-list string))
  (with-psdoc-ptr (ptr) object
    (ps_create_gstate ptr opt-list)))

(defmethod delete-doc ((object psdoc))
  (with-psdoc-ptr (ptr) object
    (ps_delete ptr)))

(defmethod findfont ((object psdoc) (font-name string) (encoding string) embed)
  (with-psdoc-ptr (ptr) object
    (ps_findfont ptr font-name encoding (truth-lisp->c embed))))

(defmethod shading ((object psdoc) (shading-type string) (x0 number) (y0 number) (x1 number) (y1 number) (c1 number) (c2 number) (c3 number) (c4 number) (options string))
  (with-psdoc-ptr (ptr) object
    (ps_shading ptr (conv-mt x0) shading-type (conv-mt y0) (conv-mt x1) (conv-mt y1) (co-sf c1) (co-sf c2) (co-sf c3) (co-sf c4) options)))

(defmethod glyph-show ((object psdoc) (glyph-name string))
  (with-psdoc-ptr (ptr) object
    (ps_glyph_show ptr glyph-name)))

(defmethod include-file ((object psdoc) (ps-filename string))
  (with-psdoc-ptr (ptr) object
    (pslib_errornum<0 (ps_include_file ptr ps-filename))))

(defmethod get-parameter ((object psdoc) (name string) &optional (modifier 0.0))
   (with-psdoc-ptr (ptr) object
     (ps_get_parameter ptr name (float modifier))))

(defmethod get-value ((object psdoc) (name string) &optional (modifier 0.0))
  (with-psdoc-ptr (ptr) object
    (ps_get_value ptr name (float modifier))))

(defmethod makespotcolor ((object psdoc) (name string) &optional (reserved 0.0))
  (with-psdoc-ptr (ptr) object
    (let ((color-id (ps_makespotcolor ptr name reserved)))
      (if (<= color-id 0)
	  (error 'spotcolor-error
		 :text (format nil "Can not set spot color with name ~a" name))
	  color-id))))

(defmethod open-image-file ((object psdoc) (type string) (file-name string) (param string) (int-param integer))
  (with-psdoc-ptr (ptr) object
    (let ((image-id (ps_open_image_file ptr type file-name param int-param)))
      (if (<= image-id 0)
	  (error 'image-load-error
		 :text (format nil "File ~a is not a valid image file of type ~a" file-name type))
	  (values image-id
		  (get-value object +value-key-imagewidth+  image-id)
		  (get-value object +value-key-imageheight+ image-id))))))

(defmethod open-image ((object psdoc) (type string)
		       (source string) (data list)
		       (length integer) (width integer)
		       (height integer)
		       (components integer)
		       (bpc integer)
		       (params string))
    (with-psdoc-ptr (ptr) object
      (with-list->foreign-array (data-arr :char #'identity) data
	  (let ((image-id (ps_open_image ptr type source data-arr length
					 width height components bpc params)))

	    (if (<= image-id 0)
		(error 'image-load-error
		       :text (format nil "Load of image from memory failed"))
		image-id)))))

(defmethod place-image ((object psdoc) (image-id integer) (x number) (y number) (scale number))
  (with-psdoc-ptr (ptr) object
    (ps_place_image ptr image-id (conv-mt x) (conv-mt y) (co-sf scale))))

(defmethod rotate ((object psdoc) (rot number))
  (with-psdoc-ptr (ptr) object
    (ps_rotate ptr (co-sf rot))))

(defmethod scale ((object psdoc) (scale-factor-x number) (scale-factor-y number))
  (with-psdoc-ptr (ptr) object
    (ps_scale ptr (co-sf scale-factor-x) (co-sf scale-factor-y))))

(defmethod set-border-link-color ((object psdoc) (r number) (g number) (b number))
  (with-psdoc-ptr (ptr) object
    (ps_set_border_color ptr (co-sf r) (co-sf g) (co-sf b))))

(defmethod set-border-link-dash ((object psdoc) (black number) (white number))
  (with-psdoc-ptr (ptr) object
    (ps_set_border_dash ptr (conv-mt black) (conv-mt white))))

(defmethod set-border-link-style ((object psdoc) (style string) (width number))
  (with-psdoc-ptr (ptr) object
    (ps_set_border_style ptr style (conv-mt width))))

(defmethod set-info ((object psdoc) (key string) (val string))
  (with-psdoc-ptr (ptr) object
    (ps_set_info ptr key val)))

(defmethod set-parameter ((object psdoc) (key string) (val string))
  (with-psdoc-ptr (ptr) object
    (ps_set_parameter ptr key val)))

(defmethod set-text-pos ((object psdoc) (x number) (y number))
  (with-psdoc-ptr (ptr) object
    (ps_set_text_pos ptr (conv-mt x) (conv-mt y))))

(defmethod set-value ((object psdoc) (key string) (val number))
  (with-psdoc-ptr (ptr) object
    (ps_set_value ptr key (co-sf val))))

(defmethod setcolor ((object psdoc) (type string)  (color-space string)
		     &optional (c1 0.0) (c2 0.0) (c3 0.0) (c4 0.0))
  (with-psdoc-ptr (ptr) object
    (ps_setcolor ptr type color-space (co-sf c1) (co-sf c2) (co-sf c3) (co-sf c4))))

(defmethod setcolor ((object psdoc) (type string) (color cl-colors:rgb)
		     &optional c1 c2 c3 c4)
  (declare (ignore c1 c2 c3 c4))
  (with-psdoc-ptr (ptr) object
    (ps_setcolor ptr type +color-space-rgb+
		 (co-sf (cl-colors:rgb-red color))
		 (co-sf (cl-colors:rgb-green color))
		 (co-sf (cl-colors:rgb-blue color))
		 1.0)))

(defmethod setflat ((object psdoc) (val number))
  (assert (<= 0.2 val 100)) ;; according to sources
  (with-psdoc-ptr (ptr) object
    (ps_setflat ptr (co-sf val))))

(defmethod setfont ((object psdoc) (font-id integer) (size number))
  (assert (> size 0))
  (with-psdoc-ptr (ptr) object
    (ps_setfont ptr font-id (conv-mt size))))

(defmethod setgray ((object psdoc) (level number))
  (assert (<= 0 level 1)) ;; according to manual
  (with-psdoc-ptr (ptr) object
    (ps_setgray ptr (co-sf level))))

(defmethod setlinecap ((object psdoc) (type integer))
  (with-psdoc-ptr (ptr) object
    (ps_setlinecap ptr type)))

(defmethod setlinejoin ((object psdoc) (type integer))
  (with-psdoc-ptr (ptr) object
    (ps_setlinejoin ptr type)))

(defmethod setlinewidth ((object psdoc) (width number))
  (with-psdoc-ptr (ptr) object
    (ps_setlinewidth ptr (conv-mt width))))

(defmethod setmiterlimit ((object psdoc) (value number))
  (with-psdoc-ptr (ptr) object
    (ps_setmiterlimit ptr (conv-mt value))))

(defmethod setpolydash ((object psdoc) (black-white list))
  (with-psdoc-ptr (ptr) object
    (with-list->foreign-array (arr :float #'(lambda(i) (conv-mt i))) black-white
      (ps_setpolydash ptr arr (length black-white)))))

(defmethod shading-pattern ((object psdoc) (shading-id integer) &optional (option-list ""))
  (with-psdoc-ptr (ptr) object
    (let ((sh-pattern-id (ps_shading_pattern ptr shading-id option-list)))
      (if (<= sh-pattern-id 0)
	  (error 'shading-pattern-error
		 :text (format nil "Shading pattern from shading-id: ~a failed." shading-id))
	  sh-pattern-id))))

(defmethod shfill ((object psdoc) (shading-id integer))
  (with-psdoc-ptr (ptr) object
    (ps_shfill ptr shading-id)))

(defmethod show ((object psdoc) (text string) &optional (x-len 0))
  (with-psdoc-ptr (ptr) object
    (if (> 0 x-len)
	(ps_show2 ptr text (round (conv-mt x-len)))
	(ps_show ptr text))))

(defmethod show-boxed ((object psdoc) (text string)
		       (left number) (top number)
		       (width number) (height number)
		       (h-mode string) (feature string))
  (with-psdoc-ptr (ptr) object
    (values
     (ps_show_boxed ptr text (conv-mt left) (conv-mt top) (conv-mt width)
		    (conv-mt height) h-mode feature)
     (get-value object +value-key-boxheight+))))

(defmethod show-xy ((object psdoc) (text string) (x number) (y number) &optional (x-len 0))
  (with-psdoc-ptr (ptr) object
    (if (> 0 x-len)
	(ps_show_xy2 ptr text (round (conv-mt x-len)) (conv-mt x) (conv-mt y))
	(ps_show_xy ptr text (conv-mt x) (conv-mt y)))))

(defclass text-metrics ()
  ((width
    :initarg :width
    :accessor width)
   (height
    :initarg :height
    :accessor height)
   (ascent
    :initarg :ascent
    :accessor ascent)
   (descent
    :initarg :descent
    :accessor descent)))

(defmethod print-object ((object text-metrics) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "metrics w: ~a h: ~a ascent: ~s descent: ~a"
	    (width object) (height object) (ascent object) (descent object))))

(defmethod string-geometry ((object psdoc) (text string) (size number) (font-id integer)
			    &key (end (length text)))
  (with-psdoc-ptr (ptr) object
    (with-list->foreign-array (data-arr :float #'identity)
	(map-into (make-list 3) #'(lambda() (float 0)))
      (ps_string_geometry ptr text end font-id size data-arr)
      (let ((metrics-list '()))
	(setf metrics-list
	      (dotimes (i 3 (reverse metrics-list))
		(push (point->millimeter (cffi:mem-aref data-arr :float i))
		      metrics-list)))
	(make-instance 'text-metrics
		       :width (first metrics-list)
		       :height size
		       :ascent (third metrics-list)
		       :descent (second metrics-list))))))

(defmethod font-symbol ((object psdoc) (char integer))
  (with-psdoc-ptr (ptr) object
    (ps_symbol ptr char)))

(defmethod font-symbol-name ((object psdoc) (idx integer) (name string)
			 &optional (font-id 0) (size (length name)))
  (with-psdoc-ptr (ptr) object
    (ps_symbol_name ptr idx font-id name size)))

(defmethod font-symbol-width ((object psdoc) (idx integer)
			 &optional (font-id 0) (size 0.0))
  (with-psdoc-ptr (ptr) object
    (ps_symbol_width ptr idx font-id size)))

(defmethod translate ((object psdoc) (x number) (y number))
  (with-psdoc-ptr (ptr) object
    (ps_translate ptr (conv-mt x) (conv-mt y))))

(defmethod curve-to ((object psdoc) p1 p2 p3)
  (curveto object
	   (conv-mt (first p1)) (conv-mt (second p1))
	   (conv-mt (first p2)) (conv-mt (second p2))
	   (conv-mt (first p3)) (conv-mt (second p3))))

(defmethod bezier-to (object p1 p2 p3 p4 &key (threshold 0.1))
  (let* ((ct-pts (mapcar #'(lambda (p) (list (conv-mt (first p)) (conv-mt (second p))))
			 (list p1 p2 p3 p4)))
	 (pairs (recursive-bezier ct-pts :threshold threshold)))
    (format t "~a~%" pairs)
    (mapcar #'(lambda (p) (lineto object (first p) (second p))) pairs)))
