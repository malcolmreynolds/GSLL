;; Chebyshev Approximations
;; Liam Healy Sat Nov 17 2007 - 20:36
;; Time-stamp: <2008-01-20 22:37:14EST chebyshev.lisp>
;; $Id: $

(in-package :gsl)

;;; Needs with- macro to allocate, initialize, and free object.

;;;;****************************************************************************
;;;; Creation and calculation of Chebyshev series
;;;;****************************************************************************

(defun-gsl allocate-chebyshev (order)
  "gsl_cheb_alloc"
  ((order :size))
  :c-return :pointer
  :documentation
  "Allocate a Chebyshev series of specified order
   and return a pointer to it.")

(defun-gsl free-chebyshev (chebyshev)
  "gsl_cheb_free"
  ((chebyshev :pointer))
  :c-return :void
  :documentation
  "Free a previously allocated Chebyshev series.")

(defun-gsl initialize-chebyshev (chebyshev function lower-limit upper-limit)
  "gsl_cheb_init"
  ((chebyshev :pointer) (function :pointer)
   (lower-limit :double) (upper-limit :double))
  :documentation
  "Compute the Chebyshev approximation for the function over the range
   (lower-limit, upper-limit) to the previously specified order.  The
   computation of the Chebyshev approximation is an @math{O(n^2)}
   process, and requires @math{n} function evaluations.")

;;;;****************************************************************************
;;;; Chebyshev series evaluation
;;;;****************************************************************************

;;; The functions that don't return are defined, but it is recommended
;;; to use the functions that do return error (and ignore it if
;;; desired) in the form of #'evaluate-chebyshev.

(defun-gsl evaluate-chebyshev-noerror (chebyshev x)
  "gsl_cheb_eval"
  ((chebyshev :pointer) (x :double))
  :c-return :double
  :index evaluate-chebyshev
  :export nil
  :documentation
  "Evaluate the Chebyshev series at a point x.")

(defun-gsl evaluate-chebyshev-noerror-order (chebyshev x order)
  "gsl_cheb_eval_n"
  ((chebyshev :pointer) (order :size) (x :double))
  :c-return :double
  :index evaluate-chebyshev
  :export nil
  :documentation
  "Evaluate the Chebyshev series at a point x to at most the given order.")

(defun-gsl evaluate-chebyshev-full (chebyshev x)
  "gsl_cheb_eval_err"
  ((chebyshev :pointer) (x :double) (result :double) (abserr :double))
  :index evaluate-chebyshev
  :export nil
  :documentation
  "Evaluate the Chebyshev series at a point x, returning result and
   an estimate of its absolute error.")

(defun-gsl evaluate-chebyshev-order (chebyshev x order)
  "gsl_cheb_eval_n_err"
  ((chebyshev :pointer) (order :size) (x :double) (result :double) (abserr :double))
  :index evaluate-chebyshev
  :export nil
  :documentation
  "Evaluate the Chebyshev series at a point x to at most the given order,
   returning result and an estimate of its absolute error.")

(export 'evaluate-chebyshev)
(defun-optionals evaluate-chebyshev (chebyshev x &optional order)
  -full -order
  "Evaluate the Chebyshev series at a point x to at most the given order,
   returning result and an estimate of its absolute error.")

;;;;****************************************************************************
;;;; Derivatives and integrals
;;;;****************************************************************************

(defun-gsl derivative-chebyshev (derivative chebyshev)
  "gsl_cheb_calc_deriv"
  ((derivative :pointer) (chebyshev :pointer))
  :documentation
  "Compute the derivative of the Chebyshev series, storing
   the derivative coefficients in the previously allocated series.
   The two series must have been allocated with the same order.")

(defun-gsl integral-chebyshev (integral chebyshev)
  "gsl_cheb_calc_integ"
  ((integral :pointer) (chebyshev :pointer))
  :documentation
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
  (let ((cheb (allocate-chebyshev 40))
	(steps 100))
    (initialize-chebyshev cheb chebyshev-step 0.0d0 1.0d0)
    (dotimes (i steps)
      (let ((x (coerce (/ i steps) 'double-float)))
	(format t "~&~a ~a ~a ~a"
		x
		(chebyshev-step x)
		(evaluate-chebyshev cheb x 10)
		(evaluate-chebyshev cheb x))))
    (free-chebyshev cheb)))

(defun chebyshev-point-example (x)
  (check-type x double-float)
  (let ((cheb (allocate-chebyshev 40))
	(deriv (allocate-chebyshev 40))
	(integ (allocate-chebyshev 40)))
    (initialize-chebyshev cheb chebyshev-step 0.0d0 1.0d0)
    (derivative-chebyshev deriv cheb)
    (integral-chebyshev integ cheb)
    (prog1
	(list
	 (evaluate-chebyshev cheb x)
	 (evaluate-chebyshev deriv x)
	 (evaluate-chebyshev integ x))
      (free-chebyshev cheb)
      (free-chebyshev deriv)
      (free-chebyshev integ))))

(lisp-unit:define-test chebyshev
  (lisp-unit:assert-equal
   '("0.715920990169d+00" "-0.150199666581d+01" "0.172397194040d+00")
   (lisp-unit:fp-sequence (chebyshev-point-example 0.55d0))))
