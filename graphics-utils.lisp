;; This software is Copyright (c) cage
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-pslib)

(defun aabb->rect (coords)
  "(upper-left-x upper-left-y bottom-right-x bottom-right-y) to
   (upper-left-x upper-left-y  w h)"
  (let ((x1 (first coords))
	(y1 (second coords))
	(x2 (third coords))
	(y2 (fourth coords)))
  (list x1 y1 (- x2 x1) (- y2 y1))))

(defun rect->aabb (coords)
  "(upper-left-x upper-left-y  w h) to
   (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  (let ((x1 (first coords))
	(y1 (second coords))
	(w (third coords))
	(h (fourth coords)))
  (list x1 y1 (+ x1 w) (+ y1 h))))

(defun inside-aabb-p (aabb x y)
  "t if x y is inside this bounding box
   aabb is in the form: (upper-left-x upper-left-y bottom-right-x bottom-right-y)"
  (and
   (> x (first aabb))
   (< x (third aabb))
   (> y (second aabb))
   (< y (fourth aabb))))


(defun line-eqn(a b &optional (thresh 1e-5))
  "Calculate a bidimensional line equation crossing vector a and b.
   Return a list containing m q and two flag indicating if the line is
   paralle to x or y respectively"
  (let ((dy (- (second b) (second a)))
	(dx (- (first b)  (first a))))
    (cond
      ((<= 0 dy thresh) ;parallel to x
       (list 0 (second b) t nil))
      ((<= 0 dx thresh) ; parallel to y
       (list 0 0 nil t))
      (t
       (list (/ dy dx) (- (second a ) (* (/ dy dx) (first a))) nil nil)))))
  
       

(defun recursive-bezier (pairs &key (threshold 1))
  (labels ((midpoint (pb pe)
	     (mapcar #'(lambda (x) (/ x 2)) (2d-vector-sum pb pe)))
	   (eqvec-p (a b) (and (= (first a) (first b))
			       (= (second a) (second b)))))

    (let* ((p1 (first pairs))
	   (p2 (second pairs))
	   (p3 (third pairs))
	   (p4 (fourth pairs))
	   (p12 (midpoint p1 p2))
	   (p23 (midpoint p2 p3))
	   (p34 (midpoint p3 p4))
	   (p12-23 (midpoint p12 p23))
	   (p23-34 (midpoint p23 p34))
	   (res (midpoint p12-23 p23-34)))
      (if (>= (2d-vector-magn (2d-vector-diff p1 res)) threshold)
	  (remove-duplicates
	   (append (list p1)
		   (recursive-bezier* (list p1 p12 p12-23 res) :threshold threshold)
		   (list res)
		   (recursive-bezier* (list res p23-34 p34 p4) :threshold threshold)
		   (list p4))
	   :test #'eqvec-p)
	  nil))))
    
    
	     

(defmacro funcall-if-not-null (func val)
  (if (not (null func))
      `(funcall ,func ,val)
      val))


(defun 2d-vector-map (v &key (funcx nil) (funcy nil))
  "Return a list of x,y values of the vector transformed by funcx and funcy (if not nil) respectively"
  (list
   (if (not (null funcx))
       (funcall-if-not-null funcx (first v))
       (funcall-if-not-null nil (first v)))

   (if (not (null funcy))
       (funcall-if-not-null funcy (second v))
       (funcall-if-not-null nil (second v)))))

(defun 2d-vector-list-map (pairs &key (funcx nil) (funcy nil))
  "Remap pairs applying funcx and funcy (if not nil) to each component"
  (mapcar #'(lambda (v) (2d-vector-map v :funcx funcx :funcy funcy)) pairs))


(defun 2d-vector-list-scale (pairs &optional (ax 1) (ay 1))
  "Remap pairs scaling each components by ax and ay"
  (mapcar #'(lambda (v) (2d-vector-scale v ax ay)) pairs))


(defun 2d-vector-list-translate (pairs &optional (dx 0) (dy 0))
  "translate pairs by dx and dy"
  (mapcar #'(lambda (v) (2d-vector-map v 
				       :funcx #'(lambda (x) (+ x dx)) 
				       :funcy #'(lambda (y) (+ y dy))))
	  pairs))

(defun 2d-vector-list-rotate (pairs angle)
  (mapcar #'(lambda (v) (2d-vector-rotate v angle)) pairs))

(defun 2d-vector-sum (a b)
  (mapcar #'(lambda (x y) (+ x y)) a b)) 

(defun 2d-vector-diff (a b)
  (mapcar #'(lambda (x y) (- x y)) a b)) 

(defun 2d-vector-dot-product (a b)
 (+ (* (first a) (first b)) (* (second a) (second b))))

(defun 2d-vector-cross-product (a b)
  (- (* (first a) (second b)) (* (second a) (first b))))

(defun 2d-vector-scale (a amount-x &optional (amount-y amount-x))
  (list (* amount-x (first a))  (* amount-y (second a))))

(defun 2d-vector-translate (a amount-x &optional (amount-y amount-x))
  (list (+ amount-x (first a))  (+ amount-y (second a))))

(defun 2d-vector-magn (a)
  (sqrt (+ (expt (first a) 2) (expt (second a) 2))))

(defun 2d-vector-normalize (a)
  (let ((mag (2d-vector-magn a)))
    (list (/ (first a) mag) (/ (second a) mag))))


(defun 2d-vector-angle (a b)
  (let* ((a-norm (2d-vector-normalize a))
	 (b-norm (2d-vector-normalize b))
	 (dot-product (2d-vector-dot-product a-norm b-norm))
	 (angle (acos dot-product)))
    
    (if (< (2d-vector-cross-product a b) 0)
	(- angle)
	angle)))


(defun 2d-vector-rotate (a angle)
  (list
   (- (* (first a) (cos angle)) (* (second a) (sin angle)))
   (+ (* (first a) (sin angle)) (* (second a) (cos angle)))))


(defun xy->pair (xs ys)
  "Convert (x1 x2 x3...) (y1 y2 y3...) to ((x1 y1) (x2 y2) (x3 y3) ...)"
  (mapcar #'(lambda (x y) (list x y)) xs ys))

(defun pair->interleaved-xy (x-y)
  "Convert ((x1 y1) (x2 y2) (x3 y3) ...) to (x1 y1 x2 y2 x3 y3 ...)"
  (reduce #'append x-y))

(defun xy->interleaved-xy (xs ys &key (modfunc-x nil) (modfunc-y nil))
  "Convert (x1 x2 x3...) (y1 y2 y3...) to ( (funcall modfunc-x x1) (funcall modfunc-y y1)...)"
  (pair->interleaved-xy (xy->pair (if (not (null modfunc-x))
				      (mapcar modfunc-x xs)
				      xs)
				  (if (not (null modfunc-y))
				      (mapcar modfunc-y ys)
				      ys))))
		
		  
(defun interleaved-xy->pair (xy)
  (macrolet ((get-from-list (when-clause list)
	       `(loop 
		   for i in ,list
		   for c = 0 then (1+ c)
		   when (,when-clause c)
		   collect i)))
    (let ((xs (get-from-list evenp xy))
	  (ys (get-from-list oddp xy)))
      (xy->pair xs ys))))
	
       
