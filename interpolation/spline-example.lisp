;********************************************************
; file:        spline-example.lisp                       
; description: Example spline                            
; date:        Sat Nov 10 2007 - 21:18                   
; author:      Liam Healy                                
; modified:    Sat Nov 10 2007 - 22:10
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Possible future improvements:
;;; Direct double-float vectors.
;;; Use generic alloc/free defined in data.lisp

(defun spline-example-arrays ()
  (let ((xarr (make-array '(10) :element-type 'double-float))
	(yarr (make-array '(10) :element-type 'double-float)))
    (loop for i from 0 below 10
	  do (setf (aref xarr i) (+ i (* 0.5d0 (sin (coerce i 'double-float))))
		   (aref yarr i) (+ i (cos (expt (coerce i 'double-float) 2)))))
    (values xarr yarr)))

(defun spline-example ()
  "The first example in Sec. 26.7 of the GSL manual."
  (let ((acc (allocate-acceleration))
	(spline (allocate-spline *cubic-spline-interpolation* 10)))
    (multiple-value-bind (xarr yarr)
	(spline-example-arrays)
      (with-data (cxarr vector-double 10)
	(with-data (cyarr vector-double 10)
	  (setf (data cxarr) xarr (data cyarr) yarr)
	  (initialize-spline spline (gsl-array cxarr) (gsl-array cyarr) 10)
	  (prog1
	      (loop for xi from (aref xarr 0) below (aref xarr 9) by 0.01d0
		    collect (list xi (evaluate-spline spline xi acc)))
	    (free-spline spline)
	    (free-acceleration acc)))))))
