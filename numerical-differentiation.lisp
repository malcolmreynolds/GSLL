;; Numerical differentiation.                
;; Liam Healy Mon Nov 12 2007 - 22:07
;; Time-stamp: <2008-02-17 18:36:19EST numerical-differentiation.lisp>
;; $Id$

(in-package :gsl)

;;; The example at the end needs to be made into a regression test.
;;; GSL function "callback" passing is identical to
;;; numerical-integration, so those definitions have been used.
;;; Some improvement could be made in naming/organization.

(defmfun central-derivative (function x step)
  "gsl_deriv_central"
  ((function :pointer) (x :double) (step :double)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Compute the numerical derivative of the function
   at the point x using an adaptive central difference algorithm with
   a step-size of step.   The derivative and an
   estimate of its absolute error is returned.

   The initial value of step is used to estimate an optimal step-size,
   based on the scaling of the truncation error and round-off error in the
   derivative calculation.  The derivative is computed using a 5-point rule
   for equally spaced abscissae at x-step, x-step/2, x,
   x+step/2, x, with an error estimate taken from the difference
   between the 5-point rule and the corresponding 3-point rule x-step,
   x, x+step.  Note that the value of the function at x
   does not contribute to the derivative calculation, so only 4-points are
   actually used.")

(defmfun forward-derivative (function x step)
  "gsl_deriv_forward"
  ((function :pointer) (x :double) (step :double)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Compute the numerical derivative of the function
   at the point x using an adaptive forward difference algorithm with
   a step-size of step.  The function is evaluated only at points greater
   than x and never at x itself.  The derivative is returned in
   result and an estimate of its absolute error is returned as the
   second value.  This function should be used if f(x) has a
   discontinuity at x, or is undefined for values less than x.

   The initial value of step is used to estimate an optimal step-size,
   based on the scaling of the truncation error and round-off error in
   the derivative calculation.  The derivative at x is computed
   using an ``open'' 4-point rule for equally spaced abscissae at
   x+step/4, x+step/2, x+3step/4, x+step,
   with an error estimate taken from the difference between the 4-point
   rule and the corresponding 2-point rule x+step/2,
   x+step.")

(defmfun backward-derivative (function x step)
  "gsl_deriv_backward"
  ((function :pointer) (x :double) (step :double)
   (result :double) (abserr :double))
  :documentation			; FDL
  "Compute the numerical derivative of the function at the point x
   using an adaptive backward difference algorithm with a step-size of
   step. The function is evaluated only at points less than x, and never
   at x itself.  The derivative is returned in result and an estimate of
   its absolute error is returned as the second value.  This function
   should be used if f(x) has a discontinuity at x, or is undefined for
   values greater than x.  This function is equivalent to calling
   #'forward-derivative with a negative step-size.")

;;;; Examples and unit test

;;; This is the example given in the GSL manual, Sec. 27.2.
(defun-single 3/2-power (x) (expt x 3/2))
;;; (3/2-power 2.0d0)

#|
(make-tests numerical-differentiation
   ;; Compare to (* 3/2 (sqrt 2.0d0))
  (central-derivative 3/2-power 2.0d0 1.d-8))
|#

(LISP-UNIT:DEFINE-TEST NUMERICAL-DIFFERENTIATION
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.121320312002221d0 4.0642813729715275d-7)
   (MULTIPLE-VALUE-LIST
    (CENTRAL-DERIVATIVE 3/2-POWER 2.0d0 1.d-8))))

