;;;;gradient.lisp
;;;;
;;;;simple gradient functions for learning graphics
(in-package :ray-tracer)

(defclass rgb-hue ()
  ((red  :initarg :red
         :accessor red)
  (green :initarg :green
         :accessor green)
  (blue  :initarg :blue
         :accessor blue)
  (alpha :initarg :alpha
         :accessor alpha)))

(defmethod print-object ((obj rgb-hue) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((red red)
		     (green green)
		     (blue blue)
		     (alpha alpha))
	obj
      (format stream "R: ~a, G: ~a, B: ~a, A: ~a" red green blue alpha))))

(defun make-hue (red green blue alpha)
  "Mixes and stores the rgb-hue."
  (make-instance 'rgb-hue :red red
		          :green green
			  :blue blue
			  :alpha alpha))

(defun draw-hue (hue width height file)
  "Draws a solid block of the specified hue."
  (let ((png (make-instance 'zpng:pixel-streamed-png
			    :color-type :truecolor-alpha
			    :width width
			    :height height)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop for i from 1 to (* width height)
	    do (zpng:write-pixel (list (red hue)
				       (green hue)
				       (blue hue)
				       (alpha hue))
				 png))
      (finish-png png))))


(defun gradient-unit (start-val end-val pixelnum)
  "Finds the unit of gradient given start value, end value, and number of pixels"
  (/ (- end-val start-val)
     pixelnum))

(defun draw-gradient (start-hue end-hue width height file)
  (let ((png (make-instance 'zpng:pixel-streamed-png
			    :color-type :truecolor-alpha
			    :width width
			    :height height))
	(r-unit (gradient-unit (red start-hue) (red end-hue) (* width height)))
	(g-unit (gradient-unit (green start-hue) (green end-hue) (* width height)))
	(b-unit (gradient-unit (blue start-hue) (blue end-hue) (* width height)))
	(a-unit (gradient-unit (alpha start-hue) (alpha end-hue) (* width height))))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop :with r-val := (red start-hue)
	    :with g-val := (green start-hue)
	    :with b-val := (blue start-hue)
	    :with a-val := (alpha start-hue)

	    :for i from 1 to (* width height)
	    :do (zpng:write-pixel (list (floor r-val)
					(floor g-val)
					(floor b-val)
					(floor a-val))
				  png)
	    :do (setf r-val (+ r-val r-unit)
		      g-val (+ g-val g-unit)
		      b-val (+ b-val b-unit)
		      a-val (+ a-val a-unit)))
      (finish-png png))))
