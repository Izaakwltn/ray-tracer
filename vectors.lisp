;;;;vectors.lisp
;;;;

(in-package :ray-tracer)

(defclass vect ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)))

;;;;maybe come back and do defgeneric make-vector
;(defgeneric make-vector (object)
 ; (:documentation "generates a vector from an object"))

(defun default-vect ()
  (make-instance 'vect :x 0.0
		       :y 1.0
		       :z 0.0))

(defun copy-vect (vect)
  (make-instance 'vect :x (x vect)
		       :y (y vect)
		       :z (z vect)))

(defun make-vect (x y z)
  (make-instance 'vect :x x
		       :y y
		       :z z))

;;;;------------------------------------------------------------------------
;;;;Math
;;;;------------------------------------------------------------------------

(defun vect-length (vect)
  (sqrt (+ (* (x vect) (x vect))
	   (* (y vect) (y vect))
	   (* (z vect) (z vect)))))

(defun add-vect (vect1 vect2)
  (make-instance 'vect
		 :x (+ (x vect1) (x vect2))
		 :y (+ (y vect1) (y vect2))
		 :z (+ (z vect1) (z vect2))))

(defun sub-vect (vect1 vect2)
  (make-instance 'vect
		 :x (- (x vect1) (x vect2))
		 :y (- (y vect1) (y vect2))
		 :z (- (z vect1) (z vect2))))

(defun dot-product (vect1 vect2)
  (+ (* (x vect1) (x vect2))
     (* (y vect1) (y vect2))
     (* (z vect1) (z vect2))))

(defun cross-product (vect)
  (format nil "I'll figure it out after lunch"))


