;; Numerical integration
;; Liam Healy, Wed Jul  5 2006 - 23:14
;; Time-stamp: <2008-08-23 19:18:03EDT numerical-integration.lisp>
;; $Id$

;;; To do: QAWS, QAWO, QAWF, more tests

(in-package :gsl)

;;;;****************************************************************************
;;;; QNG non-adaptive Gauss-Kronrod integration
;;;;****************************************************************************

(defmfun integration-QNG
    (function a b &optional (absolute-error 1.0d0) (relative-error 1.0d0))
  ;; Set absolute-error and relative-error to 1 because it apparently doesn't matter
  ;; what these are if they are too large, it will do a minimum number
  ;; of points anyway.
  "gsl_integration_qng"
  ((function :pointer)
   (a :double) (b :double)
   (absolute-error :double) (relative-error :double)
   (result :double) (abserr :double) (neval sizet))
  :documentation			; FDL
  "Apply the Gauss-Kronrod 10-point, 21-point, 43-point and
   87-point integration rules in succession until an estimate of the
   integral of f over (a,b) is achieved within the desired
   absolute and relative error limits, absolute-error and relative-error.  The
   function returns the final approximation, an estimate of
   the absolute error, and the number of function evaluations
   used.  The Gauss-Kronrod rules are designed in such a way
   that each rule uses all the results of its predecessors, in order to
   minimize the total number of function evaluations.")

;;;;****************************************************************************
;;;; QAG adaptive Gauss-Kronrod integration
;;;;****************************************************************************

(defgo-s (integration-workspace size)
	 integration-workspace-alloc integration-workspace-free)

(defmfun integration-workspace-alloc (size)
  "gsl_integration_workspace_alloc" ((size sizet))
  :c-return :pointer
  :export nil
  :index (letm integration-workspace)
  :documentation			; FDL
  "Allocate a workspace sufficient to hold n double
  precision intervals, their integration results and error estimates.")

(defmfun integration-workspace-free (pointer)
  "gsl_integration_workspace_free"
  ((mpointer :pointer))
  :c-return :void
  :export nil
  :index (letm integration-workspace)
  :documentation			; FDL
  "Free the memory associated with the workspace.")

(cffi:defcenum integrate-method
  :gauss15 :gauss21 :gauss31
  :gauss41 :gauss51 :gauss61)

(defmfun integration-QAG
    (function a b method limit workspace
	      &optional (absolute-error 1.0d0) (relative-error 1.0d0))
  ;; Set absolute-error and relative-error to 1 because it apparently doesn't matter
  ;; what these are if they are too large, it will do a minimum number
  ;; of points anyway.
  "gsl_integration_qag"
  ((function :pointer)
   (a :double) (b :double)
   (absolute-error :double) (relative-error :double)
   (limit sizet) (method integrate-method) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Apply an integration rule adaptively until an estimate
  of the integral of f over (a,b) is achieved within the
  desired absolute and relative error limits, absolute-error and
  relative-error.  The function returns the final approximation,
  and an estimate of the absolute error.  The integration rule
  is determined by the value of method, which should
  be chosen from the following symbolic names,
  :gauss15 :gauss21 :gauss31 :gauss41 :gauss51 :gauss61
  corresponding to the 15, 21, 31, 41, 51 and 61 point Gauss-Kronrod
  rules.  The higher-order rules give better accuracy for smooth functions,
  while lower-order rules save time when the function contains local
  difficulties, such as discontinuities.
  On each iteration the adaptive integration strategy bisects the interval
  with the largest error estimate.  The subintervals and their results are
  stored in the memory provided by workspace.  The maximum number of
  subintervals is given by limit, which may not exceed the allocated
  size of the workspace.")

;;;;****************************************************************************
;;;; QAGS adaptive integration with singularity
;;;;****************************************************************************

(defmfun integration-QAGS
    (function a b limit workspace
	      &optional (absolute-error 1.0d0) (relative-error 1.0d0))
  "gsl_integration_qags"
  ((function :pointer)
   (a :double) (b :double)
   (absolute-error :double) (relative-error :double) (limit sizet) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Apply the Gauss-Kronrod 21-point integration rule
   adaptively until an estimate of the integral of f over
   (a,b) is achieved within the desired absolute and relative error
   limits, absolute-error and relative-error.  The results are extrapolated
   using the epsilon-algorithm, which accelerates the convergence of the
   integral in the presence of discontinuities and integrable
   singularities.  The function returns the final approximation from the
   extrapolation, and an estimate of the absolute error.  The subintervals
   and their results are stored in the
   memory provided by workspace.  The maximum number of subintervals
   is given by limit, which may not exceed the allocated size of the
   workspace.")

;;;;****************************************************************************
;;;; QAGP adaptive integration with known singular points
;;;;****************************************************************************

(defmfun integration-QAGP
    (function points limit workspace &optional (absolute-error 1.0d0) (relative-error 1.0d0))
  "gsl_integration_qagp"
  ((function :pointer)
   ((mpointer points) :pointer) ((dim0 points) sizet)
   (absolute-error :double) (relative-error :double) (limit sizet) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Apply the adaptive integration algorithm QAGS taking
   account of the user-supplied locations of singular points.  The array
   pts of length npts should contain the endpoints of the
   integration ranges defined by the integration region and locations of
   the singularities.  For example, to integrate over the region
   (a,b) with break-points at x_1, x_2, x_3 (where 
   a < x_1 < x_2 < x_3 < b) then an array with
   (setf (data array) #(a x_1 x_2 x_3 b)) should be used.
   If you know the locations of the singular points in the integration
   region then this routine will be faster than #'integration-QAGS.")

;;;;****************************************************************************
;;;; QAGI adaptive integration on infinite intervals
;;;;****************************************************************************

(defmfun integration-QAGi
    (function limit workspace &optional (absolute-error 1.0d0) (relative-error 1.0d0))
  "gsl_integration_qagi"
  ((function :pointer)
   (absolute-error :double) (relative-error :double) (limit sizet) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Compute the integral of the function f over the
   infinite interval (-\infty,+\infty).  The integral is mapped onto the
   semi-open interval (0,1] using the transformation x = (1-t)/t,
   \int_{-\infty}^{+\infty} dx \, f(x) 
    = \int_0^1 dt \, (f((1-t)/t) + f(-(1-t)/t))/t^2.
   It is then integrated using the QAGS algorithm.  The normal 21-point
   Gauss-Kronrod rule of QAGS is replaced by a 15-point rule, because the
   transformation can generate an integrable singularity at the origin.  In
   this case a lower-order rule is more efficient.")

(defmfun integration-QAGiu
    (function a limit workspace &optional (absolute-error 1.0d0) (relative-error 1.0d0))
  "gsl_integration_qagiu"
  ((function :pointer) (a :double)
   (absolute-error :double) (relative-error :double) (limit sizet) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Compute the integral of the function f over the
   semi-infinite interval (a,+\infty).  The integral is mapped onto the
   semi-open interval (0,1] using the transformation x = a + (1-t)/t,
   int_{a}^{+\infty} dx,  f(x) = \int_0^1 dt f(a + (1-t)/t)/t^2
   and then integrated using the QAGS algorithm.")

(defmfun integration-QAGil
    (function b limit workspace &optional (absolute-error 1.0d0) (relative-error 1.0d0))
  "gsl_integration_qagil"
  ((function :pointer) (b :double)
   (absolute-error :double) (relative-error :double) (limit sizet) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Compute the integral of the function f over the
   semi-infinite interval (-\infty,b).  The integral is mapped onto the
   semi-open interval (0,1] using the transformation x = b - (1-t)/t,
   \int_{-\infty}^{b} dx, f(x) = \int_0^1 dt, f(b - (1-t)/t)/t^2
   and then integrated using the QAGS algorithm.")

;;;;****************************************************************************
;;;; QAWC adaptive integration for Cauchy principal values
;;;;****************************************************************************

(defmfun integration-QAWC
    (function a b c limit workspace
	      &optional (absolute-error 1.0d0) (relative-error 1.0d0))
  "gsl_integration_qawc"
  ((function :pointer)
   (a :double) (b :double) (c :double)
   (absolute-error :double) (relative-error :double) (limit sizet) (workspace :pointer)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Compute the Cauchy principal value of the integral of
   f over (a,b), with a singularity at c,
   I = \int_a^b dx, {f(x)/x - c} = lim_{epsilon -> 0}
   {\int_a^{c-epsilon} dx, {f(x)/x - c} + \int_{c+epsilon}^b dx,
   {f(x) \over x - c}}
   The adaptive bisection algorithm of QAG is used, with modifications to
   ensure that subdivisions do not occur at the singular point x = c.
   When a subinterval contains the point x = c or is close to
   it then a special 25-point modified Clenshaw-Curtis rule is used to control
   the singularity.  Further away from the singularity the algorithm
   uses an ordinary 15-point Gauss-Kronrod integration rule.")


;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(defun-single one-sine (x) (sin x))

#|
;;; Parameters may be defined through the lexical environment:
(let ((mult 2.0d0))
  (defun-single two-sine (x) (sin (* mult x))))
|#

#|
(make-tests numerical-integration
  (integration-qng one-sine 0.0d0 pi)
  (letm ((ws (integration-workspace 20)))
     (integration-QAG one-sine 0.0d0 pi :gauss15 20 ws))
  (letm ((ws (integration-workspace 20)))
     (integration-QAG one-sine 0.0d0 pi :gauss15 50 ws)))
|#

(LISP-UNIT:DEFINE-TEST NUMERICAL-INTEGRATION
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.0d0 2.220446049250313d-14 21)
   (MULTIPLE-VALUE-LIST
    (INTEGRATION-QNG ONE-SINE 0.0d0 PI)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.0d0 2.220446049250313d-14)
   (MULTIPLE-VALUE-LIST
    (LETM ((WS (INTEGRATION-WORKSPACE 20)))
      (INTEGRATION-QAG ONE-SINE 0.0d0 PI :GAUSS15 20
		       WS))))
  (LISP-UNIT:ASSERT-ERROR
   'GSL-CONDITION
   (LETM ((WS (INTEGRATION-WORKSPACE 20)))
     (INTEGRATION-QAG ONE-SINE 0.0d0 PI :GAUSS15 50 WS))))

