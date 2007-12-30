;********************************************************
; file:        numerical-differentiation.lisp            
; description: Numerical differentiation.                
; date:        Mon Nov 12 2007 - 22:07                   
; author:      Liam Healy                                
; modified:    Sun Dec 30 2007 - 14:25
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; The example at the end needs to be made into a regression test.
;;; GSL function "callback" passing is identical to
;;; numerical-integration, so those definitions have been used.
;;; Some improvement could be made in naming/organization.

(defun-gsl central-derivative (function x step)
  "gsl_deriv_central"
  ((function :pointer) (x :double) (step :double)
   (result :double) (abserr :double))
  :documentation
  "Compute the numerical derivative of the function
   at the point @var{x} using an adaptive central difference algorithm with
   a step-size of step.   The derivative and an
   estimate of its absolute error is returned.

   The initial value of step is used to estimate an optimal step-size,
   based on the scaling of the truncation error and round-off error in the
   derivative calculation.  The derivative is computed using a 5-point rule
   for equally spaced abscissae at @math{x-step}, @math{x-step/2}, @math{x},
   @math{x+step/2}, @math{x}, with an error estimate taken from the difference
   between the 5-point rule and the corresponding 3-point rule @math{x-step},
   @math{x}, @math{x+step}.  Note that the value of the function at @math{x}
   does not contribute to the derivative calculation, so only 4-points are
   actually used.")

(defun-gsl forward-derivative (function x step)
  "gsl_deriv_forward"
  ((function :pointer) (x :double) (step :double)
   (result :double) (abserr :double))
  :documentation
  "Compute the numerical derivative of the function
   at the point @var{x} using an adaptive forward difference algorithm with
   a step-size of step.  The function is evaluated only at points greater
   than @var{x}, and never at @var{x} itself.  The derivative is returned in
   @var{result} and an estimate of its absolute error is returned in
   @var{abserr}.  This function should be used if @math{f(x)} has a
   discontinuity at @var{x}, or is undefined for values less than @var{x}.

   The initial value of step is used to estimate an optimal step-size,
   based on the scaling of the truncation error and round-off error in
   the derivative calculation.  The derivative at @math{x} is computed
   using an ``open'' 4-point rule for equally spaced abscissae at
   @math{x+step/4}, @math{x+step/2}, @math{x+3step/4}, @math{x+step},
   with an error estimate taken from the difference between the 4-point
   rule and the corresponding 2-point rule @math{x+step/2},
   @math{x+step}.")

(defun-gsl backward-derivative (function x step)
  "gsl_deriv_backward"
  ((function :pointer) (x :double) (step :double)
   (result :double) (abserr :double))
  :documentation
  "Compute the numerical derivative of the function at the point
   @var{x} using an adaptive backward difference algorithm with a
   step-size of step. The function is evaluated only at points less than
   @var{x}, and never at @var{x} itself.  The derivative is returned in
   @var{result} and an estimate of its absolute error is returned in
   @var{abserr}.  This function should be used if @math{f(x)} has a
   discontinuity at @var{x}, or is undefined for values greater than
   @var{x}.  This function is equivalent to calling #'forward-derivative
   with a negative step-size.")

;;;; Examples and unit test

;;; This is the example given in the GSL manual, Sec. 27.2.
(defun-scalar 3/2-power (x) (expt x 3/2))
;;; (3/2-power 2.0d0)

(lisp-unit:define-test numerical-differentiation
  (lisp-unit:assert-first-fp-equal
   "0.212132031200d+01"
   ;; Compare to (* 3/2 (sqrt 2.0d0))
   (central-derivative 3/2-power 2.0d0 1.d-8)))
