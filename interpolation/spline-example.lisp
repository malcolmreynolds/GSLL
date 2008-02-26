;; Example spline
;; Liam Healy, Sat Nov 10 2007 - 21:18
;; Time-stamp: <2008-02-03 22:59:29EST spline-example.lisp>
;; $Id$

(in-package :gsl)

;;; Possible future improvement: direct double-float vectors.

(defun spline-example-arrays ()
  (let ((xarr (make-array '(10) :element-type 'double-float))
	(yarr (make-array '(10) :element-type 'double-float)))
    (loop for i from 0 below 10
	  do (setf (aref xarr i) (+ i (* 0.5d0 (sin (coerce i 'double-float))))
		   (aref yarr i) (+ i (cos (expt (coerce i 'double-float) 2)))))
    (values xarr yarr)))

(defun spline-example ()
  "The first example in Sec. 26.7 of the GSL manual."
  (multiple-value-bind (xarr yarr)
      (spline-example-arrays)
    (letm ((acc (acceleration))
	   (cxarr (vector-double xarr))
	   (cyarr (vector-double yarr))
	   (spline (spline *cubic-spline-interpolation* cxarr cyarr)))
      (loop for xi from (aref xarr 0) below (aref xarr 9) by 0.01d0
	    collect (list xi (evaluate-spline spline xi acc))))))
