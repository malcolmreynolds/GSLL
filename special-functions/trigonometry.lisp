;********************************************************
; file:        trigonometry.lisp                         
; description: Trigonometry                              
; date:        Thu May  4 2006 - 22:58                   
; author:      Liam M. Healy                             
; modified:    Mon Oct  8 2007 - 11:27
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Circular Trigonometric Functions
;;;;****************************************************************************

(defgeneric gsl-sin (x)
  (:documentation "The sine function @math{\sin(x)}."))

(defgeneric gsl-cos (x)
  (:documentation "The cosine function @math{\sin(x)}."))

(defun-gsl gsl-sin ((x float))
  "gsl_sf_sin_e" ((x :double) (ret sf-result))
  :type :method
  :export t)

(defun-gsl gsl-sin ((x complex))
  "gsl_sf_complex_sin_e"
  (((realpart x) :double) ((imagpart x) :double)
   (re-ret sf-result) (im-ret sf-result))
  :type :method
  :return ((complex (val re-ret) (val im-ret))
	   (complex (err re-ret) (err im-ret))))

(defun-gsl gsl-cos ((x float))
  "gsl_sf_cos_e" ((x :double) (ret sf-result))
  :type :method
  :export t)

(defun-gsl gsl-cos ((x complex))
  "gsl_sf_complex_cos_e"
  (((realpart x) :double) ((imagpart x) :double)
   (re-ret sf-result) (im-ret sf-result))
  :type :method 
  :return ((complex (val re-ret) (val im-ret))
	   (complex (err re-ret) (err im-ret))))

(defun-gsl hypotenuse (x y)
  "gsl_sf_hypot_e" ((x :double) (y :double) (ret sf-result))
  :documentation "The hypotenuse function @math{\sqrt@{x^2 + y^2@}}.")

(defun-gsl sinc (x)
  "gsl_sf_sinc_e" ((x :double) (ret sf-result))
  :documentation "@math{\sinc(x) = \sin(\pi x) / (\pi x)}")

(defun-gsl log-sin (x)
  "gsl_sf_complex_logsin_e"
  (((realpart x) :double) ((imagpart x) :double)
   (re-ret sf-result) (im-ret sf-result))
  :documentation "This function computes the logarithm of the complex sine,
  @math{\log(\sin(z_r + i z_i))} storing the real and imaginary parts in
  @var{szr}, @var{szi}."
  :return ((complex (val re-ret) (val im-ret))
	   (complex (err re-ret) (err im-ret))))

;;;;****************************************************************************
;;;; Hyperbolic Trigonometric Functions
;;;;****************************************************************************

(defun-gsl log-sinh (x)
  "gsl_sf_lnsinh_e" ((x :double) (ret sf-result))
  :documentation "Logarithm of sinh function, special functions
  These routines compute @math{\log(\sinh(x))} for @math{x > 0}.")

(defun-gsl log-cosh (x)
  "gsl_sf_lncosh_e" ((x :double) (ret sf-result))
  :documentation "Logarithm of cosh function, special functions
  These routines compute @math{\log(\cosh(x))} for any @var{x}.")

;;;;****************************************************************************
;;;; Conversion Functions
;;;;****************************************************************************

(defun-gsl polar-to-rectangular (r theta)
  "gsl_sf_polar_to_rect"
  ((r :double) (theta :double) (x sf-result) (y sf-result))
  :documentation "Convert the polar coordinates (@var{r},@var{theta}) to
  rectilinear coordinates (@var{x},@var{y}), @math{x = r\cos(\theta)},
  @math{y = r\sin(\theta)}."
  :return ((val x) (val y) (err x) (err y)))

(defun-gsl rectangular-to-polar (x y)
  "gsl_sf_rect_to_polar"
  ((x :double) (y :double) (r sf-result) (theta sf-result))
  :documentation "Convert the rectilinear coordinates (@var{x},@var{y}) to
  polar coordinates (@var{r},@var{theta}), such that @math{x =
  r\cos(\theta)}, @math{y = r\sin(\theta)}.  The argument @var{theta}
  lies in the range @math{[-\pi, \pi]}."
  :return ((val r) (val theta) (err r) (err theta)))

;;;;****************************************************************************
;;;; Restriction Functions
;;;;****************************************************************************

(defun-gsl restrict-symmetric (theta)
  "gsl_sf_angle_restrict_symm" ((theta :double))
  :c-return :double
  :documentation "Force the angle @var{theta} to lie in the range
  @math{(-\pi,\pi]}.")

(defun-gsl restrict-positive (theta)
  "gsl_sf_angle_restrict_pos" ((theta :double))
  :c-return :double
  :documentation "Force the angle @var{theta} to lie in
  the range @math{[0,2\pi)}.")

;;;;****************************************************************************
;;;; Trigonometric Functions With Error Estimates
;;;;****************************************************************************

(defun-gsl sin-err (x dx)
  "gsl_sf_sin_err_e" ((x :double) (dx :double) (ret sf-result))
  :documentation "Compute the sine of an angle @var{x} with
   an associated absolute error @var{dx}, @math{\sin(x \pm dx)}.")

(defun-gsl cos-err (x dx)
  "gsl_sf_cos_err_e" ((x :double) (dx :double) (ret sf-result))
  :documentation "The cosine of an angle @var{x} with an associated
  absolute error @var{dx}, @math{\cos(x \pm dx)}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test trigonometry
  (lisp-unit:assert-first-fp-equal "0.841470984808d+00" (gsl-sin 1.0d0))
  (lisp-unit:assert-equal
   '("0.129845758142d+01" "0.634963914785d+00")
   (lisp-unit::fp-string (gsl-sin #C(1.0d0 1.0d0))))
  (lisp-unit:assert-first-fp-equal "0.540302305868d+00" (gsl-cos 1.0d0))
  (lisp-unit:assert-equal
   '("0.833730025131d+00" "-0.988897705763d+00")
   (lisp-unit::fp-string (gsl-cos #C(1.0d0 1.0d0))))
  (lisp-unit:assert-first-fp-equal
   "0.223606797750d+01"
   (hypotenuse 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal "0.636619772368d+00" (sinc 0.5d0))
  (lisp-unit:assert-equal
   '("0.368383731425d+00" "0.454820233310d+00")
   (lisp-unit::fp-string (log-sin #C(1.0d0 1.0d0))))
  (lisp-unit:assert-first-fp-equal "-0.651822325947d+00" (log-sinh 0.5d0))
  (lisp-unit:assert-first-fp-equal "0.120114506958d+00" (log-cosh 0.5d0))
  (lisp-unit:assert-equal
   '("0.108060461174d+01" "0.168294196962d+01")
   (lisp-unit::fp-sequence
    (subseq (multiple-value-list
	     (polar-to-rectangular 2.0d0 1.0d0))
	    0 2)))
  (lisp-unit:assert-equal
   '("0.223606797750d+01" "0.463647609001d+00")
   (lisp-unit::fp-sequence
    (subseq (multiple-value-list (rectangular-to-polar 2.0d0 1.0d0)) 0 2)))
  (lisp-unit:assert-first-fp-equal
   "-0.128318530718d+01"
   (restrict-symmetric 5.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.528318530718d+01"
   (restrict-positive -1.0d0))
  (lisp-unit:assert-first-fp-equal "0.479425538604d+00" (sin-err 0.5d0 0.01d0))
  (lisp-unit:assert-first-fp-equal "0.877582561890d+00" (cos-err 0.5d0 0.01d0)))
