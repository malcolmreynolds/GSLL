;; Numerical differentiation.                
;; Liam Healy Mon Nov 12 2007 - 22:07
;; Time-stamp: <2009-03-02 09:25:00EST numerical-differentiation.lisp>
;; $Id$

(in-package :gsl)

;;; The example at the end needs to be made into a regression test.
;;; GSL function "callback" passing is identical to
;;; numerical-integration, so those definitions have been used.
;;; Some improvement could be made in naming/organization.

(defmfun central-derivative (function x step)
  "gsl_deriv_central"
  ((callback :pointer) (x :double) (step :double)
   (result :double) (abserr :double))
  :callbacks (callback gsl-function (function))
  :callback-dynamic ((function))
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
  ((callback :pointer) (x :double) (step :double)
   (result :double) (abserr :double))
  :callbacks (callback gsl-function (function))
  :callback-dynamic ((function))
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
  ((callback :pointer) (x :double) (step :double)
   (result :double) (abserr :double))
  :callbacks (callback gsl-function (function))
  :callback-dynamic ((function))
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

;;; Examples from gsl-1.11/deriv/test.c

(defun deriv-f1-d (x) (exp x))
(defun deriv-f2 (x) (if (not (minusp x)) (expt x 3/2) 0.0d0))
(defun deriv-f2-d (x) (if (not (minusp x)) (* 3/2 (sqrt x)) 0.0d0))
(defun deriv-f3 (x) (if (not (zerop x)) (sin (/ x)) 0.0d0))
(defun deriv-f3-d (x) (if (not (zerop x)) (/ (- (cos (/ x))) (expt x 2)) 0.0d0))
(defun deriv-f4 (x) (exp (- (expt x 2))))
(defun deriv-f4-d (x) (* -2 x (exp (- (expt x 2)))))
(defun deriv-f5 (x) (expt x 2))
(defun deriv-f5-d (x) (* 2 x))
(defun deriv-f6-d (x) (- (expt x -2)))

(save-test numerical-differentiation
 (central-derivative 'exp 1.0d0 1.0d-4)
 (forward-derivative 'exp 1.0d0 1.0d-4)
 (backward-derivative 'exp 1.0d0 1.0d-4)
 (central-derivative 'deriv-f2 0.1d0 1.0d-4)
 (forward-derivative 'deriv-f2 0.1d0 1.0d-4)
 (backward-derivative 'deriv-f2 0.1d0 1.0d-4)
 (central-derivative 'deriv-f3 0.45d0 1.0d-4)
 (forward-derivative 'deriv-f3 0.45d0 1.0d-4)
 (backward-derivative 'deriv-f3 0.45d0 1.0d-4)
 (central-derivative 'deriv-f4 0.5d0 1.0d-4)
 (forward-derivative 'deriv-f4 0.5d0 1.0d-4)
 (backward-derivative 'deriv-f4 0.5d0 1.0d-4)
 (central-derivative 'deriv-f5 0.0d0 1.0d-4)
 (forward-derivative 'deriv-f5 0.0d0 1.0d-4)
 (backward-derivative 'deriv-f5 0.0d0 1.0d-4)
 (central-derivative '/ 10.0d0 1.0d-4)
 (forward-derivative '/ 10.0d0 1.0d-4)
 (backward-derivative '/ 10.0d0 1.0d-4))
