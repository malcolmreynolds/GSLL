;; Numerical differentiation.                
;; Liam Healy Mon Nov 12 2007 - 22:07
;; Time-stamp: <2008-10-25 12:00:36EDT numerical-differentiation.lisp>
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

;;; Examples from gsl-1.11/deriv/test.c

(defun-single deriv-f1 (x) (exp x))
(defun deriv-f1-d (x) (exp x))
(defun-single deriv-f2 (x) (if (not (minusp x)) (expt x 3/2) 0.0d0))
(defun deriv-f2-d (x) (if (not (minusp x)) (* 3/2 (sqrt x)) 0.0d0))
(defun-single deriv-f3 (x) (if (not (zerop x)) (sin (/ x)) 0.0d0))
(defun deriv-f3-d (x) (if (not (zerop x)) (/ (- (cos (/ x))) (expt x 2)) 0.0d0))
(defun-single deriv-f4 (x) (exp (- (expt x 2))))
(defun deriv-f4-d (x) (* -2 x (exp (- (expt x 2)))))
(defun-single deriv-f5 (x) (expt x 2))
(defun deriv-f5-d (x) (* 2 x))
(defun-single deriv-f6 (x) (/ x))
(defun deriv-f6-d (x) (- (expt x -2)))

(central-derivative deriv-f1 1.0d0 1.0d-4)
(forward-derivative deriv-f1 1.0d0 1.0d-4)
(backward-derivative deriv-f1 1.0d0 1.0d-4)
(central-derivative deriv-f2 0.1d0 1.0d-4)
(forward-derivative deriv-f2 0.1d0 1.0d-4)
(backward-derivative deriv-f2 0.1d0 1.0d-4)
(central-derivative deriv-f3 0.45d0 1.0d-4)
(forward-derivative deriv-f3 0.45d0 1.0d-4)
(backward-derivative deriv-f3 0.45d0 1.0d-4)
(central-derivative deriv-f4 0.5d0 1.0d-4)
(forward-derivative deriv-f4 0.5d0 1.0d-4)
(backward-derivative deriv-f4 0.5d0 1.0d-4)
(central-derivative deriv-f5 0.0d0 1.0d-4)
(forward-derivative deriv-f5 0.0d0 1.0d-4)
(backward-derivative deriv-f5 0.0d0 1.0d-4)
(central-derivative deriv-f6 10.0d0 1.0d-4)
(forward-derivative deriv-f6 10.0d0 1.0d-4)
(backward-derivative deriv-f6 10.0d0 1.0d-4)

(save-test numerical-differentiation
	   (central-derivative deriv-f1 1.0d0 1.0d-4)
	   (forward-derivative deriv-f1 1.0d0 1.0d-4)
	   (backward-derivative deriv-f1 1.0d0 1.0d-4)
	   (central-derivative deriv-f2 0.1d0 1.0d-4)
	   (forward-derivative deriv-f2 0.1d0 1.0d-4)
	   (backward-derivative deriv-f2 0.1d0 1.0d-4)
	   (central-derivative deriv-f3 0.45d0 1.0d-4)
	   (forward-derivative deriv-f3 0.45d0 1.0d-4)
	   (backward-derivative deriv-f3 0.45d0 1.0d-4)
	   (central-derivative deriv-f4 0.5d0 1.0d-4)
	   (forward-derivative deriv-f4 0.5d0 1.0d-4)
	   (backward-derivative deriv-f4 0.5d0 1.0d-4)
	   (central-derivative deriv-f5 0.0d0 1.0d-4)
	   (forward-derivative deriv-f5 0.0d0 1.0d-4)
	   (backward-derivative deriv-f5 0.0d0 1.0d-4)
	   (central-derivative deriv-f6 10.0d0 1.0d-4)
	   (forward-derivative deriv-f6 10.0d0 1.0d-4)
	   (backward-derivative deriv-f6 10.0d0 1.0d-4))

(LISP-UNIT:DEFINE-TEST NUMERICAL-DIFFERENTIATION
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.718281828441488 4.123659913167868e-10)
   (MULTIPLE-VALUE-LIST
    (CENTRAL-DERIVATIVE DERIV-F1 1.0 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.7182817825298398 9.540320743340577e-7)
   (MULTIPLE-VALUE-LIST
    (FORWARD-DERIVATIVE DERIV-F1 1.0 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.7182818293827378 9.086969640080628e-7)
   (MULTIPLE-VALUE-LIST
    (BACKWARD-DERIVATIVE DERIV-F1 1.0 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.474341649024533 3.3922603135575853e-11)
   (MULTIPLE-VALUE-LIST
    (CENTRAL-DERIVATIVE DERIV-F2 0.1 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.47434164943235285 9.102230432046565e-8)
   (MULTIPLE-VALUE-LIST
    (FORWARD-DERIVATIVE DERIV-F2 0.1 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4743416462049176 8.787851752748856e-8)
   (MULTIPLE-VALUE-LIST
    (BACKWARD-DERIVATIVE DERIV-F2 0.1 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.9941773945923913 6.730277498876722e-10)
   (MULTIPLE-VALUE-LIST
    (CENTRAL-DERIVATIVE DERIV-F3 0.45 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.994177385772526 1.6952086150997503e-6)
   (MULTIPLE-VALUE-LIST
    (FORWARD-DERIVATIVE DERIV-F3 0.45 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.9941775182635064 1.568063267060477e-6)
   (MULTIPLE-VALUE-LIST
    (BACKWARD-DERIVATIVE DERIV-F3 0.45 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.7788007830653751 1.7227267499194736e-10)
   (MULTIPLE-VALUE-LIST
    (CENTRAL-DERIVATIVE DERIV-F4 0.5 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.7788007884386108 2.530006603059119e-7)
   (MULTIPLE-VALUE-LIST
    (FORWARD-DERIVATIVE DERIV-F4 0.5 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.7788007743421791 2.503617930568489e-7)
   (MULTIPLE-VALUE-LIST
    (BACKWARD-DERIVATIVE DERIV-F4 0.5 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0 6.66133814775094e-20)
   (MULTIPLE-VALUE-LIST
    (CENTRAL-DERIVATIVE DERIV-F5 0.0 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -2.6337311035821125e-26 1.606762230663824e-11)
   (MULTIPLE-VALUE-LIST
    (FORWARD-DERIVATIVE DERIV-F5 0.0 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.6337311035821125e-26 1.606762230663824e-11)
   (MULTIPLE-VALUE-LIST
    (BACKWARD-DERIVATIVE DERIV-F5 0.0 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.009999999999871223 2.6645352591887814e-12)
   (MULTIPLE-VALUE-LIST
    (CENTRAL-DERIVATIVE DERIV-F6 10.0 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.010000000266238856 4.9950606958271945e-9)
   (MULTIPLE-VALUE-LIST
    (FORWARD-DERIVATIVE DERIV-F6 10.0 1.e-4)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.010000000076196142 4.641550044799095e-9)
   (MULTIPLE-VALUE-LIST
    (BACKWARD-DERIVATIVE DERIV-F6 10.0 1.e-4))))

