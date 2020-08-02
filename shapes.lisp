;; This software is Copyright (c) cage
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package #:cl-pslib)

(defgeneric rounded-rectangle (object x y width height &key roundness))

(defgeneric regular-polygon (object radius sides &key sides-to-draw vertex-map-fn))

(defgeneric star (object radius offset sides))

(defmethod rounded-rectangle ((object psdoc)
                              (x     number) (y      number)
                              (width number) (height number)
                              &key (roundness  (* 0.1 (min width height))))
  (save object)
  (let* ((min-size      (min width height))
         (gap           (if (<= roundness
                                (/ min-size 2.0))
                            roundness
                            (* 0.1 min-size)))
         (actual-width  (- width  gap))
         (actual-height (- height gap)))
    ;; frame
    (moveto object (+ x gap)          y)
    (lineto object (+ x actual-width) y)
    (stroke object)
    (moveto object (+ x gap)          (+ y height))
    (lineto object (+ x actual-width) (+ y height))
    (stroke object)
    (moveto object x                  (+ y gap))
    (lineto object x                  (+ y actual-height))
    (stroke object)
    (moveto object (+ x width) (+ y gap))
    (lineto object (+ x width) (+ y actual-height))
    (stroke object)
    ;; arcs
    ;; b          c
    ;;  +--------+
    ;;  |        |
    ;;  |        |
    ;;  +--------+
    ;; a          d
    ;;
    ;; starting with a...
    (arc    object (+ x gap)          (+ y gap)           gap 180 270)
    (stroke object)
    ;; b
    (arc    object (+ x gap)          (+ y actual-height) gap  90 180)
    (stroke object)
    ;; c
    (arc    object (+ x actual-width) (+ y actual-height) gap   0  90)
    (stroke object)
    ;; d
    (arc    object (+ x actual-width) (+ y gap)           gap 270   0)
    (stroke object))
  (restore object))

(defmethod regular-polygon ((object psdoc) radius sides
                            &key
                              (sides-to-draw sides)
                              (vertex-map-fn (lambda (v index)
                                               (declare (ignore index))
                                               v)))
  (flet ((all-vertices ()
           (loop
              for ct from 0 below sides-to-draw
              for theta from 0.0 by (/ (* 2 pi) sides)
              collect
                (let ((raw (list (* radius (cos theta))
                                 (* radius (sin theta)))))
                  (funcall vertex-map-fn raw ct)))))
    (let ((vertices (all-vertices)))
      (assert (> (length vertices) 2))
      (let ((vertex-0 (elt vertices 0)))
        (moveto object (vec-x vertex-0) (vec-y vertex-0))
        (loop for (vertex-a vertex-b) on vertices while vertex-b do
             (lineto object (vec-x vertex-b) (vec-y vertex-b)))
        (lineto object (vec-x vertex-0) (vec-y vertex-0))
        (closepath object)))))

(defmethod star ((object psdoc) radius offset sides)
  (flet ((map-vertices (v i)
           (let ((actual-vec (2d-vector-rotate v
                                               (/ (* 2 pi)
                                                  (- (* 2 sides))))))
             (if (evenp i)
                 (2d-vector-scale actual-vec offset)
                 actual-vec))))
    (regular-polygon object radius sides :vertex-map-fn #'map-vertices)))
