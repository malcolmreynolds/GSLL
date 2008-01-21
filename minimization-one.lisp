;; Univariate minimization
;; Liam Healy Tue Jan  8 2008 - 21:02
;; Time-stamp: <2008-01-20 22:40:18EST minimization-one.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Initialization
;;;;****************************************************************************

(defun-gsl allocate-fminimizer (type)
  "gsl_min_fminimizer_alloc"
  ((type :pointer))
  :c-return :pointer
  :documentation
  "Allocate an instance of a minimizer of the given type.")

(defun-gsl set-fminimizer (minimizer function minimum lower upper)
  "gsl_min_fminimizer_set"
  ((minimizer :pointer) (function :pointer)
   (minimum :double) (lower :double) (upper :double))
  :documentation
  "Set, or reset, an existing minimizer to use the
   function and the initial search interval [lower,
   upper], with a guess for the location of the minimum.")

(defun-gsl set-fminimizer-with-values
    (minimizer function x-minimum x-lower x-upper
	       f-minimum f-lower f-upper)
  "gsl_min_fminimizer_set_with_values"
  ((minimizer :pointer) (function :pointer)
   (x-minimum :double) (x-lower :double) (x-upper :double)
   (f-minimum :double) (f-lower :double) (f-upper :double))
  :documentation
  "Set, or reset, an existing minimizer to use the
   function and the initial search interval [lower,
   upper], with a guess for the location of the minimum, using
   supplied rather than computed values of the function.")

(defun-gsl free-fminimizer (minimizer)
  "gsl_min_fminimizer_free"
  ((minimizer :pointer))
  :c-return :void
  :documentation
  "Free all the memory associated with the minimizer.")

(defun-gsl fminimizer-name (minimizer)
  "gsl_min_fminimizer_name"
  ((minimizer :pointer))
  :c-return :string
  :documentation
  "The name of the minimizer.")

(export '(with-fminimizer))
(defmacro with-fminimizer
    ((minimizer minimizer-type function minimum lower upper) &body body)
  "Create and initialize an fminimizer for one-dimensional problems,
   and clean up afterwards."
  `(let ((,minimizer (allocate-fminimizer ,minimizer-type)))
    (unwind-protect
	 (progn
	   (set-fminimizer ,minimizer ,function ,minimum ,lower ,upper)
	   ,@body)
      (free-fminimizer ,minimizer))))

;;;;****************************************************************************
;;;; Iteration
;;;;****************************************************************************

(defun-gsl iterate-fminimizer (minimizer)
  "gsl_min_fminimizer_iterate"
  ((minimizer :pointer))
  :c-return :success-continue
  :documentation
  "Perform a single iteration of the minimizer.  The following
   errors may be signalled: :EBADFUNC,
   the iteration encountered a singular point where the function or its
   derivative evaluated to infinity or NaN, or
   :FAILURE, the algorithm could not improve the current best approximation or
   bounding interval.")

(defun-gsl fminimizer-x-minimum (minimizer)
  "gsl_min_fminimizer_x_minimum"
  ((minimizer :pointer))
  :c-return :double
  :documentation
  "The current estimate of the position of the minimum for the minimizer.")

(defun-gsl fminimizer-x-lower (minimizer)
  "gsl_min_fminimizer_x_lower"
  ((minimizer :pointer))
  :c-return :double
  :documentation
  "The current lower bound of the interval for the minimizer.")

(defun-gsl fminimizer-x-upper (minimizer)
  "gsl_min_fminimizer_x_upper"
  ((minimizer :pointer))
  :c-return :double
  :documentation
  "The current upper bound of the interval for the minimizer.")

(defun-gsl fminimizer-f-minimum (minimizer)
  "gsl_min_fminimizer_f_minimum"
  ((minimizer :pointer))
  :c-return :double
  :documentation
  "The value of the function at the current estimate of the minimum for the
   minimizer.")

(defun-gsl fminimizer-f-lower (minimizer)
  "gsl_min_fminimizer_f_lower"
  ((minimizer :pointer))
  :c-return :double
  :documentation
  "The value of the function at the current estimate of the lower bound
   for the minimizer.")

(defun-gsl fminimizer-f-upper (minimizer)
  "gsl_min_fminimizer_f_upper"
  ((minimizer :pointer))
  :c-return :double
  :documentation
  "The value of the function at the current estimate of the upper bound
   for the minimizer.")

;;;;****************************************************************************
;;;; Stopping parameters
;;;;****************************************************************************

(defun-gsl min-test-interval (lower upper absolute-error relative-error)
  "gsl_min_test_interval"
  ((lower :double) (upper :double)
   (absolute-error :double) (relative-error :double))
  :c-return :success-continue		; guess that this is s-c, not s-f
  :documentation
  "Test for the convergence of the interval [lower,upper]
   with absolute error and relative error specified.
   The test returns T if the following condition is achieved:
   |a - b| < epsabs + epsrel min(|a|,|b|) 
   when the interval x = [a,b] does not include the origin.  If the
   interval includes the origin then min(|a|,|b|) is replaced by
   zero (which is the minimum value of |x| over the interval).  This
   ensures that the relative error is accurately estimated for minima close
   to the origin.

   This condition on the interval also implies that any estimate of the
   minimum x_m in the interval satisfies the same condition with respect
   to the true minimum x_m^*,
   |x_m - x_m^*| < epsabs + epsrel x_m^*
   assuming that the true minimum x_m^* is contained within the interval.")

;;;;****************************************************************************
;;;; Minimization algorithms
;;;;****************************************************************************

(defvariable *golden-section-fminimizer* "gsl_min_fminimizer_goldensection"
  "The golden section algorithm is the simplest method of bracketing
   the minimum of a function.  It is the slowest algorithm provided by the
   library, with linear convergence.

   On each iteration, the algorithm first compares the subintervals from
   the endpoints to the current minimum.  The larger subinterval is divided
   in a golden section (using the famous ratio (3-sqrt 5)/2 =
   0.3189660...) and the value of the function at this new point is
   calculated.  The new value is used with the constraint f(a') >
   f(x') < f(b') to a select new interval containing the minimum, by
   discarding the least useful point.  This procedure can be continued
   indefinitely until the interval is sufficiently small.  Choosing the
   golden section as the bisection ratio can be shown to provide the
   fastest convergence for this type of algorithm.")

(defvariable *brent-fminimizer* "gsl_min_fminimizer_brent"
  "The Brent minimization algorithm combines a parabolic
   interpolation with the golden section algorithm.  This produces a fast
   algorithm which is still robust.

   The outline of the algorithm can be summarized as follows: on each
   iteration Brent's method approximates the function using an
   interpolating parabola through three existing points.  The minimum of the
   parabola is taken as a guess for the minimum.  If it lies within the
   bounds of the current interval then the interpolating point is accepted,
   and used to generate a smaller interval.  If the interpolating point is
   not accepted then the algorithm falls back to an ordinary golden section
   step.  The full details of Brent's method include some additional checks
   to improve convergence.")

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************

;;; This is the example given in Sec. 33.8.  The results are different
;;; than given there.

(defun-single minimization-one-fn (x)
  (1+ (cos x)))

(defun minimization-one-example ()
  "Solving a minimum, the example given in Sec. 33.8 of the GSL manual."
  (let ((max-iter 100))
    (with-fminimizer
	(minimizer *brent-fminimizer* minimization-one-fn 2.0d0 0.0d0 6.0d0)
      (format t "~&iter ~6t   [lower ~24tupper] ~36tmin ~44tmin err ~54tupper-lower")
      (loop for iter from 0
	    for min = (fminimizer-x-minimum minimizer)
	    for lower = (fminimizer-x-lower minimizer)
	    for upper = (fminimizer-x-upper minimizer)
	    do (iterate-fminimizer minimizer)
	    while  (and (< iter max-iter)
			;; abs and rel error swapped in example?
			(not (min-test-interval lower upper 0.001d0 0.0d0)))
	    do
	    (format t "~&~d~6t~10,6f~18t~10,6f~28t~12,9f ~44t~10,4g ~10,4g"
		    iter lower upper
		    min (- min pi)
		    (- upper lower))))))
