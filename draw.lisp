;;;;draw.lisp
;;;for now just trying to draw simple things

(in-package :ray-tracer)
;(defun draw-black (file)
 ; (let* ((png (make-instance 'zpng:png
;			    :color-type :grayscale-alpha
;			    :width 200
;			    :height 200))
;	(image (data-array png))
;	 (max 255))
 ;   (

(defun draw-rgb (file)
  (let ((png (make-instance 'zpng:pixel-streamed-png
			    :color-type :truecolor-alpha
			    :width 200
			    :height 200)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
  (zpng:start-png png stream)
    (loop for a from 38 to 255 by 31
	  do (loop for b from 10 to 255 by 10
		   do (loop for g from 38 to 255 by 31
			    do (loop for r from 10 to 255 by 10
				     do (zpng:write-pixel (list r g b a) png)))))
      (zpng:finish-png png))))

(defun draw-black (file)
  (let ((png (make-instance 'zpng:pixel-streamed-png
			    :color-type :truecolor-alpha
			    :width 200
			    :height 200)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop for i from 1 to (* 200 200)
	    do (zpng:write-pixel (list 0 0 0 255) png))
      (finish-png png))))

(defun polar-bear-snow-storm (file)
  (let ((png (make-instance 'zpng:pixel-streamed-png
			    :color-type :truecolor-alpha
			    :width 200
			    :height 200)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop for i from 1 to (* 200 200)
	    do (zpng:write-pixel (list 255 255 255 255) png))
      (finish-png png))))

;;;gradient model
;;;go from one color to another, I guess change by a ratio from the number of rows?
			    
	     
    
