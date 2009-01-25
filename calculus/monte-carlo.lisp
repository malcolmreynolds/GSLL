;; Monte Carlo Integration
;; Liam Healy Sat Feb  3 2007 - 17:42
;; Time-stamp: <2009-01-25 10:21:38EST monte-carlo.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_monte.h
;;; /usr/include/gsl/gsl_monte_plain.h
;;; /usr/include/gsl/gsl_monte_miser.h
;;; /usr/include/gsl/gsl_monte_vegas.h

;;;;****************************************************************************
;;;; PLAIN Monte Carlo
;;;;****************************************************************************

(defmobject monte-carlo-plain
    "gsl_monte_plain"
  ((dim sizet))
  "plain Monte Carlo integration"
  :documentation			; FDL
  "Make and initialize a workspace for Monte Carlo integration in dim dimensions."
  :initialize-suffix "init"
  :initialize-args nil)

(cffi:defcstruct plain-state
  (dim sizet)
  (x :pointer))

(defmfun monte-carlo-integrate-plain
    (function lower-limits upper-limits calls generator state)
  "gsl_monte_plain_integrate"
  ((function :pointer)
   ((c-pointer lower-limits) :pointer) ((c-pointer upper-limits) :pointer)
   ((dim0 lower-limits) sizet) (calls sizet)
   ((mpointer generator) :pointer)
   ((mpointer state) :pointer)
   (result :double) (abserr :double))
  :inputs (lower-limits upper-limits)
  :documentation			; FDL
  "Uses the plain Monte Carlo algorithm to integrate the
   function f over the hypercubic region defined by the
   lower and upper limits in the arrays 'lower-limits and
   'upper-limits, each a gsl-vector of length dim.
   The integration uses a fixed number
   of function calls calls, and obtains random sampling points using
   the random number generator 'generator. A previously allocated workspace
   'state must be supplied.  The result of the integration is returned
   with an estimated absolute error.")

;;;;****************************************************************************
;;;; MISER
;;;;****************************************************************************

;;; The MISER algorithm of Press and Farrar is based on recursive
;;; stratified sampling.  This technique aims to reduce the overall
;;; integration error by concentrating integration points in the
;;; regions of highest variance.

(defmobject monte-carlo-miser
    "gsl_monte_miser"
  ((dim sizet))
  "miser Monte Carlo integration"
  :documentation			; FDL
  "Make and initialize a workspace for Monte Carlo integration in
   dim dimensions.  The workspace is used to maintain
   the state of the integration."
  :initialize-suffix "init"
  :initialize-args nil)

(cffi:defcstruct miser-state
  (min-calls sizet)
  (min-calls-per-bisection sizet)
  (dither :double)
  (estimate-frac :double)
  (alpha :double)
  (dim sizet)
  (estimate-style :int)
  (depth :int)
  (verbose :int)
  (x :pointer)
  (xmid :pointer)
  (sigma-l :pointer)
  (sigma-r :pointer)
  (fmax-l :pointer)
  (fmax-r :pointer)
  (fmin-l :pointer)
  (fmin-r :pointer)
  (fsum-l :pointer)
  (fsum-r :pointer)
  (fsum2-l :pointer)
  (fsum2-r :pointer)
  (hits-l :pointer)
  (hits-r :pointer))

(export 'miser-parameter)
(defmacro miser-parameter (workspace parameter)
  ;; FDL
  "Get or set with setf the parameter value for the MISER Monte Carlo
   integration method."
  ;; (miser-parameter ws min-calls)
  ;; (setf (miser-parameter ws min-calls) 300)
 `(foreign-slot-value ,workspace 'miser-state ',parameter))

(defmfun monte-carlo-integrate-miser
    (function lower-limits upper-limits calls generator state)
  "gsl_monte_miser_integrate"
  ((function :pointer)
   ((c-pointer lower-limits) :pointer) ((c-pointer upper-limits) :pointer)
   ((dim0 lower-limits) sizet) (calls sizet)
   ((mpointer generator) :pointer)
   ((mpointer state) :pointer)
   (result :double) (abserr :double))
  :inputs (lower-limits upper-limits)
  :documentation			; FDL
  "Uses the miser Monte Carlo algorithm to integrate the
   function f over the hypercubic region defined by the
   lower and upper limits in the arrays 'lower-limits and
   'upper-limits, each a gsl-vector of the samelength
   The integration uses a fixed number
   of function calls calls, and obtains random sampling points using
   the random number generator 'generator. A previously allocated workspace
   'state must be supplied.  The result of the integration is returned
   with an estimated absolute error.")

;;;;****************************************************************************
;;;; VEGAS
;;;;****************************************************************************

;;; The vegas algorithm of Lepage is based on importance sampling.  It
;;; samples points from the probability distribution described by the
;;; function |f|, so that the points are concentrated in the regions
;;; that make the largest contribution to the integral.

(defmobject monte-carlo-vegas
    "gsl_monte_vegas"
  ((dim sizet))
  "vegas Monte Carlo integration"
  :documentation			; FDL
  "Make and initialize a workspace for Monte Carlo integration in
   dim dimensions.  The workspace is used to maintain
   the state of the integration.  Returns a pointer to vegas-state."
  :initialize-suffix "init"
  :initialize-args nil)

(cffi:defcstruct vegas-state
  ;; grid 
  (dim sizet)
  (bins-max sizet)
  (bins :uint)				; uint
  (boxes :uint)		       ; these are both counted along the axes
  (xi :pointer)
  (xin :pointer)
  (delx :pointer)
  (weight :pointer)
  (vol :double)
  (x :pointer)
  (bin :pointer)
  (box :pointer)
  (d :pointer)				; distribution
  ;; control variables
  (alpha :double)
  (mode :int)
  (verbose :int)
  (iterations :uint)
  (stage :int)
  ;; scratch variables preserved between calls to vegas1/2/3
  (jac :double)
  (wtd-int-sum :double)
  (sum-wgts :double)
  (chi-sum :double)
  (chisq :double)
  (result :double)
  (sigma :double)
  (it-start :uint)
  (it-num :uint)
  (samples :uint)
  (calls-per-box :uint)
  (ostream :pointer))

(export 'vegas-parameter)
(defmacro vegas-parameter (workspace parameter)
  ;; FDL
  "Get or set with setf the parameter value for the VEGAS Monte Carlo
   integration method."
  ;; (vegas-parameter ws bins-max)
  ;; (setf (vegas-parameter ws bins-max) 300)
 `(foreign-slot-value ,workspace 'vegas-state ',parameter))

(defmfun monte-carlo-integrate-vegas
    (function lower-limits upper-limits calls generator state)
  "gsl_monte_vegas_integrate"
  ((function :pointer)
   ((c-pointer lower-limits) :pointer) ((c-pointer upper-limits) :pointer)
   ((dim0 lower-limits) sizet) (calls sizet)
   ((mpointer generator) :pointer)
   ((mpointer state) :pointer)
   (result :double) (abserr :double))
  :inputs (lower-limits upper-limits)
  :documentation			; FDL
  "Uses the vegas Monte Carlo algorithm to integrate the
   function f over the dim-dimensional hypercubic region
   defined by the lower and upper limits in the arrays x1 and
   xu, each of the same length.  The integration uses a fixed number
   of function calls calls, and obtains random sampling points using
   the random number generator r.  A previously allocated workspace
   s must be supplied.  The result of the integration is returned
   with an estimated absolute error.  The result
   and its error estimate are based on a weighted average of independent
   samples. The chi-squared per degree of freedom for the weighted average
   is returned via the state struct component, s->chisq, and must be
   consistent with 1 for the weighted average to be reliable.")

;;;;****************************************************************************
;;;; Callback definition
;;;;****************************************************************************

(cffi:defcstruct monte-function
  (function :pointer)
  (dimensions sizet)
  (parameters :pointer))

(export 'make-monte-carlo-function)
(defmacro make-monte-carlo-function (name-or-lambda &optional (dimensions 1))
  "Define the function for use in the Monte Carlo functions.  The function
   should be specified with either the name of a CL function already defined,
   or as a lambda form.  In the latter case, the number of dimensions need
   not be specified, they will be computed."
  `(make-single-function ,name-or-lambda :double :double monte-function
			 ,dimensions nil nil))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

;;; Example from Sec. 23.5
;;; This is a function that occurs in random walk studies.

(defparameter *monte-carlo-cb*
  (make-monte-carlo-function
   (lambda (x y z)
     (* (/ (expt pi 3))
	(/ (- 1 (* (cos x) (cos y) (cos z))))))))

(defun random-walk-plain-example (&optional (nsamples 500000))
  (let ((ws (make-monte-carlo-plain 3))
	(lower #m(0.0d0 0.0d0 0.0d0))
	(upper (make-marray 'double-float :initial-contents (list pi pi pi)))
	(rng (make-random-number-generator *mt19937* 0)))
    (monte-carlo-integrate-plain *monte-carlo-cb* lower upper nsamples rng ws)))

(defun random-walk-miser-example (&optional (nsamples 500000))
  (let ((ws (make-monte-carlo-miser 3))
	(lower #m(0.0d0 0.0d0 0.0d0))
	(upper (make-marray 'double-float :initial-contents (list pi pi pi)))
	(rng (make-random-number-generator *mt19937* 0)))
    (monte-carlo-integrate-miser *monte-carlo-cb* lower upper nsamples rng ws)))

(defun random-walk-vegas-example (&optional (nsamples 500000))
  (let ((ws (make-monte-carlo-vegas 3))
	(lower #m(0.0d0 0.0d0 0.0d0))
	(upper (make-marray 'double-float :initial-contents (list pi pi pi)))
	(rng (make-random-number-generator *mt19937* 0)))
    (monte-carlo-integrate-vegas *monte-carlo-cb* lower upper nsamples rng ws)))

(save-test monte-carlo
  (random-walk-plain-example)
  (random-walk-miser-example)
  (random-walk-vegas-example))
