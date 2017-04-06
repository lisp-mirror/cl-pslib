;; This software is Copyright (c) cage
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package cl-pslib)

(defmacro defcond (type)
  `(define-condition ,(alexandria:format-symbol t "TEXT-~a" (string-upcase type))
       (,type)
       ((text
         :initarg :text
         :reader text))
     (:documentation "Error that set text")))


(defcond error)
(defcond warning)

(define-condition bookmark-error (text-error)
  ()
  (:report (lambda (condition stream)
            (format stream "~a" (text condition)))))

(define-condition spotcolor-error (text-error)
  ()
  (:report (lambda (condition stream)
            (format stream "~a" (text condition)))))

(define-condition image-load-error (text-error)
  ()
  (:report (lambda (condition stream)
            (format stream "~a" (text condition)))))

(define-condition shading-pattern-error (text-error)
  ()
  (:report (lambda (condition stream)
            (format stream "~a" (text condition)))))


(define-condition not-implemented-error (text-error)
  ()
  (:documentation "Error for not-implemented features"))

(define-condition null-reference (text-error)
  ()
  (:documentation "Null reference"))


(define-condition out-of-bounds (error)
  ((seq
    :initarg :seq
    :reader seq)
   (idx
    :initarg :idx
    :reader idx))
   (:documentation "Error when you go out of bound"))
