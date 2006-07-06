;********************************************************
; file:        numerical-integration.lisp                
; description: Numerical integration                     
; date:        Wed Jul  5 2006 - 23:14                   
; author:      Liam M. Healy                             
; modified:    Thu Jul  6 2006 - 00:24
;********************************************************
;;; $Id: $

(in-package :gsl)

(cffi:defcstruct gsl-function
  "Passing functions to GSL."
  ;; see /usr/include/gsl/gsl_math.h
  (function :pointer)
  (params :pointer))

;; add &rest parameters to pass along to the function
(defun-gsl integration-QNG
    (function a b &optional (epsabs 1.0d0) (epsrel 1.0d0))
  ;; Set epsabs and epsrel to 1 because it apparently doesn't matter
  ;; what these are if they are too large, it will do a minimum number
  ;; of iterations anyway.
  "gsl_integration_qng"
  (((with-foreign-object (fn 'gsl-function)
      (setf (foreign-slot-value fn 'gsl-function 'function)
	    (get-callback function)
	    (foreign-slot-value fn 'gsl-function 'params)
	    (cffi:null-pointer))
      fn)
    :pointer)
   (a :double) (b :double)
   (epsabs :double) (epsrel :double)
   (result :double) (abserr :double) (neval :size))
  :documentation
  "Apply the Gauss-Kronrod 10-point, 21-point, 43-point and
   87-point integration rules in succession until an estimate of the
   integral of @math{f} over @math{(a,b)} is achieved within the desired
   absolute and relative error limits, @var{epsabs} and @var{epsrel}.  The
   function returns the final approximation, @var{result}, an estimate of
   the absolute error, @var{abserr} and the number of function evaluations
   used, @var{neval}.  The Gauss-Kronrod rules are designed in such a way
   that each rule uses all the results of its predecessors, in order to
   minimize the total number of function evaluations.")

(cffi:defcallback f-sine :double ((x :double) (parms :pointer))
  (declare (ignore parms))
  (sin x))

#|
(integration-qng 'f-sine 0.0d0 pi 1.0d-8 1.0d-8)
2.0d0
2.220446049250313d-14
21
|#
