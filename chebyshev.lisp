;; Chebyshev Approximations
;; Liam Healy Sat Nov 17 2007 - 20:36
;; Time-stamp: <2008-12-23 21:46:27EST chebyshev.lisp>
;; $Id$

(in-package :gsl)

#|
(defmobject chebyshev "gsl_cheb"
  ((order sizet))
  "Chebyshev series"			; FDL
  "Make a Chebyshev series of specified order."
  "init"
  ((function :pointer) (lower-limit :double) (upper-limit :double)))
|#

;;;;****************************************************************************
;;;; Creation and calculation of Chebyshev series
;;;;****************************************************************************

(defgo-s (chebyshev order function lower-limit upper-limit)
	 allocate-chebyshev free-chebyshev initialize-chebyshev)

(defmfun allocate-chebyshev (order)
  "gsl_cheb_alloc"
  ((order sizet))
  :c-return :pointer
  :export nil
  :index (letm chebyshev)
  :documentation			; FDL
  "Allocate a Chebyshev series of specified order
   and return a pointer to it.")

(defmfun free-chebyshev (chebyshev)
  "gsl_cheb_free"
  ((chebyshev :pointer))
  :c-return :void
  :export nil
  :index (letm chebyshev)
  :documentation			; FDL
  "Free a previously allocated Chebyshev series.")

(defmfun initialize-chebyshev (chebyshev function lower-limit upper-limit)
  "gsl_cheb_init"
  ((chebyshev :pointer) (function :pointer)
   (lower-limit :double) (upper-limit :double))
  :export nil
  :index (letm chebyshev)
  :documentation			; FDL
  "Compute the Chebyshev approximation for the function over the range
   (lower-limit, upper-limit) to the previously specified order.  The
   computation of the Chebyshev approximation is an O(n^2)
   process, and requires n function evaluations.")

;;;;****************************************************************************
;;;; Chebyshev series evaluation
;;;;****************************************************************************

;;; The functions that don't return are defined, but it is recommended
;;; to use the functions that do return error (and ignore it if
;;; desired) in the form of #'evaluate-chebyshev.

(defmfun evaluate-chebyshev (chebyshev x &optional order)
  ("gsl_cheb_eval" "gsl_cheb_eval_n")
  (((chebyshev :pointer) (x :double))
  ((chebyshev :pointer) (order sizet) (x :double)))
  :c-return :double
  :documentation			; FDL
  "Evaluate the Chebyshev series at a point x.  If order is supplied,
  evaluate to at most the given order.")

(defmfun evaluate-chebyshev-error (chebyshev x &optional order)
  ("gsl_cheb_eval_err" "gsl_cheb_eval_n_err")
  (((chebyshev :pointer) (x :double) (result :double) (abserr :double))
   ((chebyshev :pointer) (order sizet) (x :double) (result :double) (abserr :double)))
  :documentation			; FDL
  "Evaluate the Chebyshev series at a point x, returning result and
   an estimate of its absolute error.  If order is supplied,
   evaluate to at most the given order.")

;;;;****************************************************************************
;;;; Derivatives and integrals
;;;;****************************************************************************

(defmfun derivative-chebyshev (derivative chebyshev)
  "gsl_cheb_calc_deriv"
  ((derivative :pointer) (chebyshev :pointer))
  :documentation			; FDL
  "Compute the derivative of the Chebyshev series, storing
   the derivative coefficients in the previously allocated series.
   The two series must have been allocated with the same order.")

(defmfun integral-chebyshev (integral chebyshev)
  "gsl_cheb_calc_integ"
  ((integral :pointer) (chebyshev :pointer))
  :documentation			; FDL
  "Compute the integral of the Chebyshev series, storing
   the integral coefficients in the previously allocated series.
   The two series must have been allocated with the same order.
   The lower limit of the integration is taken to be the left hand
   end of the range lower-limit.")

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************

;;; From Chap. 28.5, except I have set steps = 100 instead of 10000
;;; to keep things sane.

(defun-single chebyshev-step (x) (if (< x 0.5d0) 0.25d0 0.75d0))

(defun chebyshev-table-example ()
  (let ((steps 100))
    (letm ((cheb (chebyshev 40 chebyshev-step 0.0d0 1.0d0)))
      (dotimes (i steps)
	(let ((x (coerce (/ i steps) 'double-float)))
	  (format t "~&~a ~a ~a ~a"
		  x
		  (chebyshev-step x)
		  (evaluate-chebyshev cheb x 10)
		  (evaluate-chebyshev cheb x)))))))

(defun chebyshev-point-example (x)
  (check-type x double-float)
  (letm ((cheb (chebyshev 40 chebyshev-step 0.0d0 1.0d0))
	 (deriv (chebyshev 40))
	 (integ (chebyshev 40)))
    (derivative-chebyshev deriv cheb)
    (integral-chebyshev integ cheb)
    (list
     (evaluate-chebyshev cheb x)
     (evaluate-chebyshev deriv x)
     (evaluate-chebyshev integ x))))

;;; Unit test
(save-test chebyshev
  (chebyshev-point-example 0.55d0))
