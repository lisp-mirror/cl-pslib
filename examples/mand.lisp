;; Cl-pslib example
;; Copyright (C) 2012  cage

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-pslib)

(defparameter *output* "pslib-mand-examples.ps")

(defparameter *line-width* 8.0)

(defparameter *page-size* +a4-page-size+)

(defparameter *color* cl-colors2:+firebrick+)

(defparameter *rect-size* 0.1) ; 0.1 mm

(defparameter *max-iter* 1000)

(defparameter *c-plane-step* 0.007)

(defparameter *scale* (/ (width *page-size*) 5))

(defparameter *colors* (list
			+orange1+
			+orange2+
			+orange3+
			+orange4+
			+darkorange1+
			+darkorange2+
			+darkorange3+
			+darkorange4+
			+coral1+
			+coral2+
			+coral3+
			+coral4+
			+tomato1+
			+tomato2+
			+tomato3+
			+tomato4+
			+orangered1+
			+orangered2+
			+orangered3+
			+orangered4+
			+red1+
			+red2+
			+red3+
			+red4+
			+debianred+))

(defun escape (c)
  (do* ((ct 0 (1+ ct))
	(p (+ (expt 0 2) c) (+ (expt z 2) c))
	(z 0 p))
       ((not (<= (abs p) 2)) ct)
    (if (>= ct *max-iter*)
	(return-from escape ct))))

(let ((doc (make-instance 'psdoc :page-size *page-size*)))
  (open-doc doc  *output*)
  (begin-page doc)
  (save doc)
  (translate doc (/ (width (page-size doc)) 2)
	     (/ (height (page-size doc)) 2))
  (scale doc *scale* *scale*)
  (do ((x -2.5 (+ *c-plane-step* x)))
      ((not (< x 2.5)))
    (do ((y -1 (+ *c-plane-step*  y)))
	((not (< y 1)))
      (let ((col (nth (mod (escape (complex x y)) (length *colors*)) *colors*)))
	(setcolor doc +color-type-fillstroke+ col)
	(rect doc x y *rect-size* *rect-size*)
	(fill-path doc))))
  (restore doc)
  (end-page doc)
  (close-doc doc)
  (shutdown))
