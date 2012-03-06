;; This software is Copyright (c) cage
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL


(in-package :cl-pslib)

(alexandria:define-constant +iso216-rate+ (/ 1 (/ 1 1.4142)) :test #'=) 
(alexandria:define-constant +1/iso216-rate+ (/ 1 +iso216-rate+) :test #'=) 

(eval-when (:compile-toplevel)
  (defun gen-papersize-iso216-list (prefix start-size)
    (loop for i from 0 to 10 and
       size = start-size then (* (/ size 2) +iso216-rate+) collect
	 (list (alexandria:format-symbol t "~a~a" prefix i) (/ size +iso216-rate+)))))




(defmacro define-iso216-papersize (fun prefix size)
  `(progn
     ,@(mapcar #'(lambda(name-width)
		   `(progn
		     (alexandria:define-constant 
			  ,(alexandria:format-symbol t "+~:@(~a~)-PAPER-WIDTH+" (first name-width))
			  ,(floor (second name-width)) :test #'=)
		     (alexandria:define-constant 
			  ,(alexandria:format-symbol t "+~:@(~a~)-PAPER-HEIGHT+" (first name-width))
			  ,(floor (* (second name-width) +iso216-rate+)) :test #'=)))
	       (funcall fun prefix size))))

(eval-when (:load-toplevel)
  (define-iso216-papersize gen-papersize-iso216-list "A" 1189)
  (define-iso216-papersize gen-papersize-iso216-list "B" 1414)
  (define-iso216-papersize gen-papersize-iso216-list "C" 1414))

		  
(eval-when (:load-toplevel :compile-toplevel)
  (defclass page-size ()
    ((width
      :initform +a4-paper-width+
      :accessor width
      :initarg :width)
     (height
      :initform +a4-paper-height+
      :accessor height
      :initarg :height)))


  (defun page-size-equal-p (p1 p2)
    (if (and
	 (= (width p1) (width p2))
	 (= (height p1) (height p2)))
	t
	nil))

  
  (defmethod print-object ((object page-size) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "width: ~s height: ~s" (width object) (height object)))))


(eval-when (:load-toplevel)
  (defmacro define-iso216-constants-page-size (prefix)
    `(progn
       ,@(loop for i from 0 to 10 collect
	      `(alexandria:define-constant 
		   ,(alexandria:format-symbol t "+~@:(~a~a-page-size~)+" prefix i)
		   (make-instance 'page-size 
				  :width ,(alexandria:format-symbol t "+~@:(~a~a-paper-width~)+" prefix i)
				  :height ,(alexandria:format-symbol t "+~@:(~a~a-paper-height~)+" prefix i))
		 :test #'page-size-equal-p))))
  

  
  (define-iso216-constants-page-size A))


(defun millimiter->centimeter (mm)
  (/ mm 10))

(defun millimiter->inch (mm)
  (centimeter->inch (millimiter->centimeter mm)))

(defun millimiter->point (mm)
  (inch->point (centimeter->inch (millimiter->centimeter mm))))

(defun centimeter->millimeter (cm)
  (* cm 10))

(defun centimeter->inch (centimeters)
  (/ centimeters 2.54))

(defun centimeter->point (centimeters)
  (inch->point (centimeter->inch centimeters)))

(defun inch->millimeter (inches)
  (centimeter->millimeter (inch->centimeter inches)))

(defun inch->centimeter (inches)
  (* inches 2.54))

(defun inch->point (inches)
  (/ inches 1/72))


(defun point->millimeter (pts)
  (centimeter->millimeter (inch->centimeter (point->inch pts))))

(defun point->centimeter (pts)
  (inch->centimeter (point->inch pts)))

(defun point->inch (pts)
  (/ pts 72))

