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

(defclass point ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)))

(defmethod print-object ((obj vect) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((x x)
		     (y y)
		     (z z))
	obj
      (format stream "(~a, ~a, ~a)" x y z))))

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
;;;;Math for either points or vects
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

(defun scalar-mult (n vect)
  (make-instance 'vect :x (* (x vect) n)
		       :y (* (y vect) n)
		       :z (* (z vect) n)))

(defun dot-product (vect1 vect2)
  (+ (* (x vect1) (x vect2))
     (* (y vect1) (y vect2))
     (* (z vect1) (z vect2))))

(defun cross-product (vect1 vect2)
  (make-instance 'vect :x (- (* (y vect1) (z vect2))
			     (* (z vect1) (y vect2)))
		       :y (- (* (z vect1) (x vect2))
			     (* (x vect1) (z vect2)))
		       :z (- (* (x vect1) (y vect2))
			     (* (y vect1) (x vect2)))))


;;;;------------------------------------------------------------------------
;;;;Points
;;;;------------------------------------------------------------------------

(defclass point ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)))

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((x x) (y y) (z z))
	obj
      (format stream "(~a, ~a, ~a)" x y z))))

(defun default-point ()
  (make-instance 'point :x 0.0
		       :y 1.0
		       :z 0.0))

(defun copy-point (point) ;or vect
  (make-instance 'point :x (x point)
		        :y (y point)
		        :z (z point)))

(defun make-point (x y z)
  (make-instance 'point :x x
		        :y y
		        :z z))
