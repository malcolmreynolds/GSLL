;********************************************************
; file:        numerical-integration.lisp                
; description: Numerical integration                     
; date:        Wed Jul  5 2006 - 23:14                   
; author:      Liam M. Healy                             
; modified:    Sun Feb 11 2007 - 11:05
;********************************************************
;;; $Id: $

;;; To do: QAWS, QAWO, QAWF, more tests

(in-package :gsl)

(cffi:defcstruct gsl-function
  "Passing functions to GSL."
  ;; see /usr/include/gsl/gsl_math.h
  (function :pointer)
  (params :pointer))

(export 'def-gsl-function)
(defmacro def-gsl-function (name arg &body body)
  "Define a GSL (C) function of either one argument of type
   double (if arg is a symbol), or a C array of doubles
   (if arg is a list), for GSL numerical
   integration functions.
   Parameters (non integration variables) may be passed by
   using a lexical closure."
  (let ((argvec (gensym "MCARG")))
    `(cffi:defcallback ,name :double
      (,(if (listp arg)
	    `(,argvec :pointer)
	    `(,arg :double))
       (params :pointer))
      (declare (ignore params))
      ,@(if (listp arg)
	    `((symbol-macrolet
		    ,(loop for i from 0 for a in arg
			   collect `(,a (cffi:mem-aref ,argvec :double ,i)))
		  ,@body))
	    body))))

(defun make-gsl-function (function)
  "Make a function for GSL to integrate."
  ;; Is this creating and deallocating the gsl-function object?
  (cffi:with-foreign-object (fn 'gsl-function)
    (setf (cffi:foreign-slot-value fn 'gsl-function 'function)
	  (cffi:get-callback function)
	  (cffi:foreign-slot-value fn 'gsl-function 'params)
	  (cffi:null-pointer))
    fn))

;;;;****************************************************************************
;;;; QNG non-adaptive Gauss-Kronrod integration
;;;;****************************************************************************

(defun-gsl integration-QNG
    (function a b &optional (epsabs 1.0d0) (epsrel 1.0d0))
  ;; Set epsabs and epsrel to 1 because it apparently doesn't matter
  ;; what these are if they are too large, it will do a minimum number
  ;; of points anyway.
  "gsl_integration_qng"
  (((make-gsl-function function) :pointer)
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
      (integration-workspace-free ,workspace))))

(cffi:defcenum integrate-method
  :gauss15 :gauss21 :gauss31
  :gauss41 :gauss51 :gauss61)

(defun-gsl integration-QAG
    (function a b method limit workspace
	      &optional (epsabs 1.0d0) (epsrel 1.0d0))
  ;; Set epsabs and epsrel to 1 because it apparently doesn't matter
  ;; what these are if they are too large, it will do a minimum number
  ;; of points anyway.
  "gsl_integration_qag"
  (((make-gsl-function function) :pointer)
   (a :double) (b :double)
   (epsabs :double) (epsrel :double)
   (limit :size) (method integrate-method) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation
  "Apply an integration rule adaptively until an estimate
  of the integral of @math{f} over @math{(a,b)} is achieved within the
  desired absolute and relative error limits, @var{epsabs} and
  @var{epsrel}.  The function returns the final approximation,
  and an estimate of the absolute error.  The integration rule
  is determined by the value of @var{method}, which should
  be chosen from the following symbolic names,
  :gauss15 :gauss21 :gauss31 :gauss41 :gauss51 :gauss61
  corresponding to the 15, 21, 31, 41, 51 and 61 point Gauss-Kronrod
  rules.  The higher-order rules give better accuracy for smooth functions,
  while lower-order rules save time when the function contains local
  difficulties, such as discontinuities.
  On each iteration the adaptive integration strategy bisects the interval
  with the largest error estimate.  The subintervals and their results are
  stored in the memory provided by @var{workspace}.  The maximum number of
  subintervals is given by @var{limit}, which may not exceed the allocated
  size of the workspace.")

;;;;****************************************************************************
;;;; QAGS adaptive integration with singularity
;;;;****************************************************************************

(defun-gsl integration-QAGS
    (function a b limit workspace &optional (epsabs 1.0d0) (epsrel 1.0d0))
  "gsl_integration_qags"
  (((make-gsl-function function) :pointer)
   (a :double) (b :double)
   (epsabs :double) (epsrel :double) (limit :size) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation
  "Apply the Gauss-Kronrod 21-point integration rule
   adaptively until an estimate of the integral of @math{f} over
   @math{(a,b)} is achieved within the desired absolute and relative error
   limits, @var{epsabs} and @var{epsrel}.  The results are extrapolated
   using the epsilon-algorithm, which accelerates the convergence of the
   integral in the presence of discontinuities and integrable
   singularities.  The function returns the final approximation from the
   extrapolation, @var{result}, and an estimate of the absolute error,
   @var{abserr}.  The subintervals and their results are stored in the
   memory provided by @var{workspace}.  The maximum number of subintervals
   is given by @var{limit}, which may not exceed the allocated size of the
   workspace.")

;;;;****************************************************************************
;;;; QAGP adaptive integration with known singular points
;;;;****************************************************************************

(defun-gsl integration-QAGP
    (function points limit workspace &optional (epsabs 1.0d0) (epsrel 1.0d0))
  "gsl_integration_qagp"
  (((make-gsl-function function) :pointer)
   ((pointer points) :pointer) ((dim0 points) :size)
   (epsabs :double) (epsrel :double) (limit :size) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation
  "Apply the adaptive integration algorithm QAGS taking
   account of the user-supplied locations of singular points.  The array
   @var{pts} of length @var{npts} should contain the endpoints of the
   integration ranges defined by the integration region and locations of
   the singularities.  For example, to integrate over the region
   @math{(a,b)} with break-points at @math{x_1, x_2, x_3} (where 
   @math{a < x_1 < x_2 < x_3 < b}) then an array with
   (setf (data array) #(a x_1 x_2 x_3 b)) should be used.
   If you know the locations of the singular points in the integration
   region then this routine will be faster than @code{QAGS}.")

;;;;****************************************************************************
;;;; QAGI adaptive integration on infinite intervals
;;;;****************************************************************************

(defun-gsl integration-QAGi
    (function limit workspace &optional (epsabs 1.0d0) (epsrel 1.0d0))
  "gsl_integration_qagi"
  (((make-gsl-function function) :pointer)
   (epsabs :double) (epsrel :double) (limit :size) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation
  "Compute the integral of the function @var{f} over the
   infinite interval @math{(-\infty,+\infty)}.  The integral is mapped onto the
   semi-open interval @math{(0,1]} using the transformation @math{x = (1-t)/t},
   \int_{-\infty}^{+\infty} dx \, f(x) 
    = \int_0^1 dt \, (f((1-t)/t) + f(-(1-t)/t))/t^2.
   It is then integrated using the QAGS algorithm.  The normal 21-point
   Gauss-Kronrod rule of QAGS is replaced by a 15-point rule, because the
   transformation can generate an integrable singularity at the origin.  In
   this case a lower-order rule is more efficient.")

(defun-gsl integration-QAGiu
    (function a limit workspace &optional (epsabs 1.0d0) (epsrel 1.0d0))
  "gsl_integration_qagiu"
  (((make-gsl-function function) :pointer) (a :double)
   (epsabs :double) (epsrel :double) (limit :size) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation
  "Compute the integral of the function @var{f} over the
   semi-infinite interval @math{(a,+\infty)}.  The integral is mapped onto the
   semi-open interval @math{(0,1]} using the transformation @math{x = a + (1-t)/t},
   \int_{a}^{+\infty} dx \, f(x) = \int_0^1 dt \, f(a + (1-t)/t)/t^2
   and then integrated using the QAGS algorithm.")

(defun-gsl integration-QAGil
    (function b limit workspace &optional (epsabs 1.0d0) (epsrel 1.0d0))
  "gsl_integration_qagil"
  (((make-gsl-function function) :pointer) (b :double)
   (epsabs :double) (epsrel :double) (limit :size) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation
  "Compute the integral of the function @var{f} over the
   semi-infinite interval @math{(-\infty,b)}.  The integral is mapped onto the
   semi-open interval @math{(0,1]} using the transformation @math{x = b - (1-t)/t},
   \int_{-\infty}^{b} dx \, f(x) = \int_0^1 dt \, f(b - (1-t)/t)/t^2
   and then integrated using the QAGS algorithm.")

;;;;****************************************************************************
;;;; QAWC adaptive integration for Cauchy principal values
;;;;****************************************************************************

(defun-gsl integration-QAWC
    (function a b c limit workspace &optional (epsabs 1.0d0) (epsrel 1.0d0))
  "gsl_integration_qawc"
  (((make-gsl-function function) :pointer)
   (a :double) (b :double) (c :double)
   (epsabs :double) (epsrel :double) (limit :size) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation
  "Compute the Cauchy principal value of the integral of
   @math{f} over @math{(a,b)}, with a singularity at @var{c},
   I = \int_a^b dx\, {f(x) \over x - c} = \lim_{\epsilon \to 0} 
   \left\{\int_a^{c-\epsilon} dx\, {f(x) \over x - c} +
   \int_{c+\epsilon}^b dx\, {f(x) \over x - c} \right\}
   The adaptive bisection algorithm of QAG is used, with modifications to
   ensure that subdivisions do not occur at the singular point @math{x = c}.
   When a subinterval contains the point @math{x = c} or is close to
   it then a special 25-point modified Clenshaw-Curtis rule is used to control
   the singularity.  Further away from the
   singularity the algorithm uses an ordinary 15-point Gauss-Kronrod
   integration rule.")


;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(let ((mult 2.0d0))
  (def-gsl-function two-sine x (sin (* mult x))))

(def-gsl-function one-sine x (sin x))

(lisp-unit:define-test numerical-integration
  (lisp-unit:assert-first-fp-equal
   "0.200000000000d+01"
   (integration-qng 'one-sine 0.0d0 pi))
  (lisp-unit:assert-first-fp-equal
   "0.200000000000d+01"
   (with-integration-workspace (ws 20)
     (integration-QAG 'one-sine 0.0d0 pi :gauss15 20 ws)))
  (lisp-unit:assert-error
   'gsl-error
   (with-integration-workspace (ws 20)
     (integration-QAG 'one-sine 0.0d0 pi :gauss15 50 ws))))
