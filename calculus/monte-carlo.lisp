;; Monte Carlo Integration
;; Liam Healy Sat Feb  3 2007 - 17:42
;; Time-stamp: <2009-04-01 21:25:08EDT monte-carlo.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_monte.h
;;; /usr/include/gsl/gsl_monte_plain.h
;;; /usr/include/gsl/gsl_monte_miser.h
;;; /usr/include/gsl/gsl_monte_vegas.h

;;;;****************************************************************************
;;;; Callback definition
;;;;****************************************************************************

(cffi:defcstruct monte-function
  (function :pointer)
  (dimensions sizet)
  (parameters :pointer))

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

(defparameter *monte-carlo-default-samples-per-dimension* 150000)

(defmfun monte-carlo-integrate-plain
    (function lower-limits upper-limits 
	      &optional
	      (number-of-samples
	       (* *monte-carlo-default-samples-per-dimension*
		  (dim0 lower-limits)))
	      (generator (make-random-number-generator +mt19937+ 0))
	      (state (make-monte-carlo-plain (dim0 lower-limits)))
	      (scalars t))
  "gsl_monte_plain_integrate"
  ((callback :pointer)
   ((c-pointer lower-limits) :pointer) ((c-pointer upper-limits) :pointer)
   ((dim0 lower-limits) sizet) (number-of-samples sizet)
   ((mpointer generator) :pointer)
   ((mpointer state) :pointer)
   (result :double) (abserr :double))
  :inputs (lower-limits upper-limits)
  :callbacks
  (callback monte-function (dimensions)
	    (function :double (:input :double :cvector dim0) :slug))
  :callback-dynamic (((dim0 lower-limits)) (function scalars))
  :documentation			; FDL
  "Uses the plain Monte Carlo algorithm to integrate the
   function f over the hypercubic region defined by the
   lower and upper limits in the arrays 'lower-limits and
   'upper-limits, each a gsl-vector of length dim.
   The integration uses a fixed number
   of function calls number-of-samples, and obtains random sampling points using
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
 `(cffi:foreign-slot-value ,workspace 'miser-state ',parameter))

(defmfun monte-carlo-integrate-miser
    (function lower-limits upper-limits 
	      &optional
	      (number-of-samples
	       (* *monte-carlo-default-samples-per-dimension*
		  (dim0 lower-limits)))
	      (generator (make-random-number-generator +mt19937+ 0))
	      (state (make-monte-carlo-miser (dim0 lower-limits)))
	      (scalars t))
  "gsl_monte_miser_integrate"
  ((callback :pointer)
   ((c-pointer lower-limits) :pointer) ((c-pointer upper-limits) :pointer)
   ((dim0 lower-limits) sizet) (number-of-samples sizet)
   ((mpointer generator) :pointer)
   ((mpointer state) :pointer)
   (result :double) (abserr :double))
  :inputs (lower-limits upper-limits)
  :callbacks
  (callback monte-function (dimensions)
	    (function :double (:input :double :cvector dim0) :slug))
  :callback-dynamic (((dim0 lower-limits)) (function scalars))
  :documentation			; FDL
  "Uses the miser Monte Carlo algorithm to integrate the
   function f over the hypercubic region defined by the
   lower and upper limits in the arrays 'lower-limits and
   'upper-limits, each a gsl-vector of the samelength
   The integration uses a fixed number
   of function calls number-of-samples, and obtains random sampling points using
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
 `(cffi:foreign-slot-value ,workspace 'vegas-state ',parameter))

(defmfun monte-carlo-integrate-vegas
    (function lower-limits upper-limits 
	      &optional
	      (number-of-samples
	       (* *monte-carlo-default-samples-per-dimension*
		  (dim0 lower-limits)))
	      (generator (make-random-number-generator +mt19937+ 0))
	      (state (make-monte-carlo-vegas (dim0 lower-limits)))
	      (scalars t))
  "gsl_monte_vegas_integrate"
  ((callback :pointer)
   ((c-pointer lower-limits) :pointer) ((c-pointer upper-limits) :pointer)
   ((dim0 lower-limits) sizet) (number-of-samples sizet)
   ((mpointer generator) :pointer)
   ((mpointer state) :pointer)
   (result :double) (abserr :double))
  :inputs (lower-limits upper-limits)
  :callbacks
  (callback monte-function (dimensions)
	    (function :double (:input :double :cvector dim0) :slug))
  :callback-dynamic (((dim0 lower-limits)) (function scalars))
  :documentation			; FDL
  "Uses the vegas Monte Carlo algorithm to integrate the function f
   over the dim-dimensional hypercubic region defined by the lower and
   upper limits in the arrays x1 and xu, each of the same length.  The
   integration uses a fixed number of function calls
   number-of-samples, and obtains random sampling points using the
   random number generator r.  A previously allocated workspace s must
   be supplied.  The result of the integration is returned with an
   estimated absolute error.  The result and its error estimate are
   based on a weighted average of independent samples. The chi-squared
   per degree of freedom for the weighted average is returned via the
   state struct component, s->chisq, and must be consistent with 1 for
   the weighted average to be reliable.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

;;; Example from Sec. 23.5

(defun mcrw (x y z)
  "Example function for Monte Carlo used in random walk studies."
  (* (/ (expt pi 3))
     (/ (- 1 (* (cos x) (cos y) (cos z))))))

(defparameter *mc-lower* #m(0.0d0 0.0d0 0.0d0))

(defparameter *mc-upper*
  (make-marray 'double-float :initial-contents (list pi pi pi)))

(defun random-walk-plain-example (&optional (nsamples 500000))
  (monte-carlo-integrate-plain 'mcrw *mc-lower* *mc-upper* nsamples))

(defun random-walk-miser-example (&optional (nsamples 500000))
  (monte-carlo-integrate-miser 'mcrw *mc-lower* *mc-upper* nsamples))

(defun random-walk-vegas-example (&optional (nsamples 500000))
  (monte-carlo-integrate-vegas 'mcrw *mc-lower* *mc-upper* nsamples))

(save-test monte-carlo
  (random-walk-plain-example)
  (random-walk-miser-example)
  (random-walk-vegas-example))
