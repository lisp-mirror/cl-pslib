;; This software is Copyright (c) cage
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL


(asdf:defsystem #:cl-pslib
  :version "0.0.1"
  :license "LLGPL"
  :depends-on (:alexandria
               :cffi
	       :cl-colors
	       :cl-ppcre)
  :components ((:file "package")
	       (:file "conditions"
		      :depends-on ("package"))
	       (:file "cffi-interface"
		      :depends-on ("package"))
	       (:file "pslib"
		      :depends-on ("cffi-interface"
				   "package"))
	       (:file "constants"
		      :depends-on ("package"
				   "pslib"))
	       (:file "page-metrics"
		      :depends-on ("package"))
               (:file "graphics-utils"
		      :depends-on ("package"))
	       (:file "cl-pslib"
		      :depends-on ("constants"
				   "conditions"
				   "page-metrics"
				   "graphics-utils"
				   "pslib"))))

