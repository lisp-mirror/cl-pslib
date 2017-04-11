;; This software is Copyright (c) cage
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-pslib)

(define-foreign-library libps
    (:darwin "libps.dylib")
  (:unix (:or "libps.so.0" "libps.so"))
  (t (:default "libps")))

(use-foreign-library libps)

(defctype size :unsigned-int)

;size_t (*writeproc)(PSDoc *p, void *data, size_t size))

(defparameter *callback-string* (string ""))

(cffi:defcallback write-to-string size ((doc :pointer) (data :pointer) (size size))
  (declare (ignore doc))
  (setf *callback-string* (concatenate 'string *callback-string*
                                       (foreign-string-to-lisp data :count size)))
  size)

(defmacro with-list->foreign-array ((arr type &optional (fun #'identity)) lst &body body)
  (alexandria:with-gensyms (ct data)
    `(cffi:with-foreign-object (,arr ,type (length ,lst))
       (loop
          for ,ct   from 0
          for ,data in ,lst do
            (setf (mem-aref ,arr ,type ,ct)
                  (funcall  ,fun ,data)))
       ,@body)))

(defmacro with-vector->foreign-array ((arr type &optional (fun #'identity)) vec &body body)
  (alexandria:with-gensyms (ct data)
    `(cffi:with-foreign-object (,arr ,type (length ,vec))
       (loop
          for ,ct   from 0
          for ,data across ,vec do
            (setf (mem-aref ,arr ,type ,ct)
                  (funcall  ,fun ,data)))
       ,@body)))

(defun pslib_errornum<0 (num)
  (if (< 0 num)
      nil
      t))

(defun pslib_errornum<=0 (num)
  (if (<= 0 num)
      nil
      t))

(defun truth-lisp->c (val)
  (if val
      1
      0))

(defun truth-c->lisp (val)
  (if (= 0 val)
      nil
      t))
