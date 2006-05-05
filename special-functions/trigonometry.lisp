;********************************************************
; file:        trigonometry.lisp                         
; description: Trigonometry                              
; date:        Thu May  4 2006 - 22:58                   
; author:      Liam M. Healy                             
; modified:    Thu May  4 2006 - 23:41
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; complex gsl-sin, gsl-cos, log-sin, need to return complex.
;;; restrict-symmetric, restrict-positive cause memory fault.

;;;;****************************************************************************
;;;; Circular Trigonometric Functions
;;;;****************************************************************************

(defgeneric gsl-sin (x)
  (:documentation "The sine function @math{\sin(x)}."))

(defgeneric gsl-cos (x)
  (:documentation "The cosine function @math{\sin(x)}."))

(defun-gsl gsl-sin ((x :double))
  "gsl_sf_sin_e"
  :method ((x double-float))
  :return (sf-result))

(defun-gsl gsl-cos ((x :double))
  "gsl_sf_cos_e"
  :method ((x double-float))
  :return (sf-result))

(defun-gsl hypotenuse ((x :double) (y :double))
  "gsl_sf_hypot_e"
  :documentation "The hypotenuse function @math{\sqrt@{x^2 + y^2@}}."
  :return (sf-result))

(defun-gsl sinc ((x :double))
  "gsl_sf_sinc_e"
  :documentation "@math{\sinc(x) = \sin(\pi x) / (\pi x)}"
  :return (sf-result))

;;; Return complex
(defun-gsl gsl-sin (((realpart x) :double) ((imagpart x) :double))
  "gsl_sf_complex_sin_e"
  :method ((x complex))
  :return (sf-result sf-result))

(defun-gsl gsl-cos (((realpart x) :double) ((imagpart x) :double))
  "gsl_sf_complex_cos_e"
  :method ((x complex))
  :return (sf-result sf-result))

(defun-gsl log-sin (((realpart x) :double) ((imagpart x) :double))
  "gsl_sf_complex_logsin_e"
  :method ((x complex))
  :documentation "This function computes the logarithm of the complex sine,
  @math{\log(\sin(z_r + i z_i))} storing the real and imaginary parts in
  @var{szr}, @var{szi}."
  :return (sf-result sf-result))

;;;;****************************************************************************
;;;; Hyperbolic Trigonometric Functions
;;;;****************************************************************************

(defun-gsl log-sinh ((x :double))
  "gsl_sf_lnsinh_e"
  :documentation "Logarithm of sinh function, special functions
  These routines compute @math{\log(\sinh(x))} for @math{x > 0}."
  :return (sf-result))

(defun-gsl log-cosh ((x :double))
  "gsl_sf_lncosh_e"
  :documentation "Logarithm of cosh function, special functions
  These routines compute @math{\log(\cosh(x))} for any @var{x}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Conversion Functions
;;;;****************************************************************************

(defun-gsl polar-to-rectangular ((r :double) (theta :double))
  "gsl_sf_polar_to_rect"
  :documentation "Convert the polar coordinates (@var{r},@var{theta}) to
  rectilinear coordinates (@var{x},@var{y}), @math{x = r\cos(\theta)},
  @math{y = r\sin(\theta)}."
  :return (sf-result sf-result))

(defun-gsl rectangular-to-polar ((x :double) (y :double))
  "gsl_sf_rect_to_polar"
  :documentation "Convert the rectilinear coordinates (@var{x},@var{y}) to
  polar coordinates (@var{r},@var{theta}), such that @math{x =
  r\cos(\theta)}, @math{y = r\sin(\theta)}.  The argument @var{theta}
  lies in the range @math{[-\pi, \pi]}."
  :return (sf-result sf-result))

;;;;****************************************************************************
;;;; Restriction Functions
;;;;****************************************************************************

;;; memory fault
(defun-gsl restrict-symmetric ((theta :double))
  "gsl_sf_angle_restrict_symm_e"
  :documentation "Force the angle @var{theta} to lie in the range
  @math{(-\pi,\pi]}."
  :return (sf-result))

(defun-gsl restrict-positive ((theta :double))
  "gsl_sf_angle_restrict_pos_e"
  :documentation "Force the angle @var{theta} to lie in
  the range @math{[0,2\pi)}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Trigonometric Functions With Error Estimates
;;;;****************************************************************************

(defun-gsl sin-err ((x :double) (dx :double))
  "gsl_sf_sin_err_e"
  :documentation "Compute the sine of an angle @var{x} with
   an associated absolute error @var{dx}, @math{\sin(x \pm dx)}."
  :return (sf-result))

(defun-gsl cos-err ((x :double) (dx :double))
  "gsl_sf_cos_err_e"
  :documentation "The cosine of an angle @var{x} with an associated
  absolute error @var{dx}, @math{\cos(x \pm dx)}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test trigonometry
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.841470984808d+00" (GSL-SIN 1.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.540302305868d+00" (GSL-COS 1.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.223606797750d+01" (HYPOTENUSE 1.0d0 2.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.636619772368d+00" (SINC 0.5d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "-0.651822325947d+00" (LOG-SINH 0.5d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.120114506958d+00" (LOG-COSH 0.5d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.479425538604d+00" (SIN-ERR 0.5d0 0.01d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.877582561890d+00" (COS-ERR 0.5d0 0.01d0))
  )
