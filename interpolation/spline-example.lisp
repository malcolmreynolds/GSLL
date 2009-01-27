;; Example spline
;; Liam Healy, Sat Nov 10 2007 - 21:18
;; Time-stamp: <2009-01-26 22:43:41EST spline-example.lisp>
;; $Id$

(in-package :gsl)

(defun spline-example (&optional (step 0.01d0))
  "The first example in Sec. 26.7 of the GSL manual."
  (let* ((acc (make-acceleration))
	 (xarr
	  (make-marray
	   'double-float
	   :initial-contents
	   (loop for i from 0.0d0 below 10.0d0
	      collect (+ i (* 0.5d0 (sin i))))))
	 (yarr
	  (make-marray
	   'double-float
	   :initial-contents
	   (loop for i from 0.0d0 below 10.0d0
	      collect (+ i (cos (expt i 2))))))
	 (spline (make-spline *cubic-spline-interpolation* xarr yarr)))
    (loop for xi from (maref xarr 0) below (maref xarr 9) by step
       collect (list xi (evaluate-spline spline xi acc)))))

(save-test interpolation (spline-example 0.1d0))
