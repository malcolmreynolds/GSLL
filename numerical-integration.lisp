;********************************************************
; file:        numerical-integration.lisp                
; description: Numerical integration                     
; date:        Wed Jul  5 2006 - 23:14                   
; author:      Liam M. Healy                             
; modified:    Thu Jul  6 2006 - 23:07
;********************************************************
;;; $Id: $

(in-package :gsl)

(cffi:defcstruct gsl-function
  "Passing functions to GSL."
  ;; see /usr/include/gsl/gsl_math.h
  (function :pointer)
  (params :pointer))

(export 'def-gsl-function)
(defmacro def-gsl-function
    (name integration-variable &body body)
  "Define a GSL function of one double-float to be used in
   numerical integration functions.  Parameters (non
   integration variables) may be passed by using a lexical closure. "
  `(cffi:defcallback ,name :double
       ((,integration-variable :double) (params :pointer))
     (declare (ignore params))
     ,@body))

;;;;****************************************************************************
;;;; QNG non-adaptive Gauss-Kronrod integration
;;;;****************************************************************************

(defun-gsl integration-QNG
    (function a b &optional (epsabs 1.0d0) (epsrel 1.0d0))
  ;; Set epsabs and epsrel to 1 because it apparently doesn't matter
  ;; what these are if they are too large, it will do a minimum number
  ;; of points anyway.
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
   function returns the final approximation, an estimate of
   the absolute error, and the number of function evaluations
   used, @var{neval}.  The Gauss-Kronrod rules are designed in such a way
   that each rule uses all the results of its predecessors, in order to
   minimize the total number of function evaluations.")

;;;;****************************************************************************
;;;; QAG adaptive Gauss-Kronrod integration
;;;;****************************************************************************

(defun-gsl integration-workspace-alloc (size)
  "gsl_integration_workspace_alloc" ((size :size))
  :c-return :pointer
  :documentation "Allocate a workspace sufficient to hold @var{n} double
  precision intervals, their integration results and error estimates.")

(defun-gsl integration-workspace-free (pointer)
  "gsl_integration_workspace_free" ((pointer :pointer))
  :c-return :void
  :documentation "Free the memory associated with the workspace @var{w}.")

(export 'with-integration-workspace)
(defmacro with-integration-workspace ((workspace size) &body body)
  `(let ((,workspace (integration-workspace-alloc ,size)))
     (unwind-protect
	  (progn
	    ,@body)
       (integration-workspace-free workspace)))))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(let ((mult 2.0d0))
  (def-gsl-function two-sine x (sin (* mult x))))

(def-gsl-function one-sine x (sin x))


#|
(integration-qng 'one-sine 0.0d0 pi)
2.0d0
2.220446049250313d-14
21
|#
