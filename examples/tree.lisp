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

(defparameter *output* "pslib-tree-examples.ps")
(defparameter *trunk-color* cl-colors:+firebrick+)
(defparameter *leaves-color* (list cl-colors:+yellow+ cl-colors:+violetred+ cl-colors:+green+))
(defparameter *line-width* 8.0)
(defparameter *page-size* +a4-page-size+)
(defparameter *trunk* '(0 110))
(defparameter *leaf-thrs* 2.0)

(defun draw-branch (doc parent branch line-width)
  (let ((tr-br (2d-vector-translate branch (first parent) (second parent)))
	(magn (2d-vector-magn branch)))
    (save doc)
    (when (> line-width 1.5)
      (setlinewidth doc line-width))
    (setcolor doc +color-type-stroke+ 
	      (if (<= magn *leaf-thrs*) 
		  (nth (random (length *leaves-color*)) *leaves-color*)
		  *trunk-color*))
    (moveto doc (first parent) (second parent))
    (lineto doc (first tr-br) (second tr-br))
    (stroke doc)
    (restore doc)))

(defun node (doc sofar &optional (trunk *trunk*) (line-width *line-width*))
  (let* ((mag (/ (2d-vector-magn trunk) (+ 1.1 (random 2.0))))
	 (angle (2d-vector-angle '(0.0 10.0) trunk))
	 (branch-num (+ 3 (random 2)))
	 (branches (loop for i from 0 to branch-num collect
			(2d-vector-rotate (list 0 mag)
					  (+ (* (expt -1 (1+ (random 2)))) angle
					     (random (/ (/ pi 2) (1+ (random 4)))))))))
    (when (>= mag (/ *leaf-thrs* 2))
      (when (eq trunk *trunk*)
	(draw-branch doc '(0 0) trunk line-width))
      (mapc #'(lambda (br)
		(draw-branch doc (2d-vector-sum sofar trunk) br line-width)
		(node doc (2d-vector-sum trunk sofar) br (- line-width 2)))
	    branches))))



(let ((doc (make-instance 'psdoc :page-size *page-size*)))
  (open-doc doc  *output*)
  (begin-page doc)
  (save doc)
  (translate doc (float (/ (width (page-size doc)) 2)) 0.0)
  (node  doc '(0 0.1))
  (restore doc)
  (end-page doc)
  (close-doc doc)
  (shutdown))

