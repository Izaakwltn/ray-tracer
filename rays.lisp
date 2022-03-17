;;;;rays.lisp
;;;;

(in-package :ray-tracer)

(defvar ray-min 0.0001)

(defvar ray-max (* 1.0 (expt 10 30)))

(defclass ray ()
  ((origin    :initarg :origin
	      :accessor origin)
   (direction :initarg :direction
	      :accessor direction)
   (t-max     :initarg :t-max
	      :initform ray-max
	      :accessor t-max)))

(defmethod print-object ((obj ray) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((origin origin)
		     (direction direction)
		     (t-max t-max))
	obj
      (format stream "Origin: (~a, ~a, ~a), Direction: (~a, ~a, ~a), T-Max: ~a"
	      (x origin) (y origin) (z origin) (x direction) (y direction) (z direction) t-max))))

(defun default-ray ()
  (make-instance 'ray :origin (make-instance 'point :x 0.0
						    :y 0.0
						    :z 0.0)
		      :direction (make-instance 'vect :x 0.0
						      :y 0.0
						      :z 0.0)))

(defun copy-ray (ray)
  (make-instance 'ray :origin (origin ray)
		      :direction (direction ray)
		      :t-max (t-max ray)))

(defun make-ray (origin direction &optional (t-max ray-max))
  (make-instance 'ray :origin origin
		      :direction direction
		      :t-max t-max))

;;;;math

(defun calculate-point (ray distance)
  (copy-point (add-vect (origin ray) (scalar-mult distance (direction ray)))))

;;;;------------------------------------------------------------------------
;;;;Intersections
;;;;------------------------------------------------------------------------

(defclass inter-section ()
  ((ray      :initarg :ray
	     :accessor ray)
   (distance :initarg :distance
	     :accessor distance)
   (shape    :initarg :shape
	     :accessor shape)))

(defmethod print-object ((obj inter-section) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((ray ray)
		     (distance distance)
		     (shape shape))
	obj
      (format stream "~a ~a ~a" ray distance shape))))

(defun default-intsec ()
  (make-instance 'inter-section :ray (default-ray)
		                :distance ray-max
				:shape nil))

(defun copy-intsec (intsec)
  (make-instance 'inter-section :ray (ray intsec)
		                :distance (distance intsec)
				:shape (shape intsec)))

(defun make-intsec (ray)
  (make-instance 'inter-section :ray ray
		                :distance (t-max ray)
				:shape nil))

;;;;------------------------------------------------------------------------
;;;;intersection functions
;;;;------------------------------------------------------------------------

(defun intersectp (intsec)
  (not (nullp (shape intsec))))

(defun find-position (intsec)
  (calculate-point (ray intsec) (distance intsec)))
