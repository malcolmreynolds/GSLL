;; Trigonometry
;; Liam Healy, Thu May  4 2006 - 22:58
;; Time-stamp: <2008-02-17 18:30:19EST trigonometry.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Circular Trigonometric Functions
;;;;****************************************************************************

(defgeneric gsl-sin (x)
  (:documentation "The sine function sin(x)."))

(defgeneric gsl-cos (x)
  (:documentation "The cosine function cos(x)."))

(defmfun gsl-sin ((x float))
  "gsl_sf_sin_e" ((x :double) (ret sf-result))
  :type :method
  :export t)

(defmfun gsl-sin ((x complex))
  "gsl_sf_complex_sin_e"
  (((realpart x) :double) ((imagpart x) :double)
   (re-ret sf-result) (im-ret sf-result))
  :type :method
  :return ((complex (val re-ret) (val im-ret))
	   (complex (err re-ret) (err im-ret))))

(defmfun gsl-cos ((x float))
  "gsl_sf_cos_e" ((x :double) (ret sf-result))
  :type :method
  :export t)

(defmfun gsl-cos ((x complex))
  "gsl_sf_complex_cos_e"
  (((realpart x) :double) ((imagpart x) :double)
   (re-ret sf-result) (im-ret sf-result))
  :type :method 
  :return ((complex (val re-ret) (val im-ret))
	   (complex (err re-ret) (err im-ret))))

(defmfun hypotenuse (x y)
  "gsl_sf_hypot_e" ((x :double) (y :double) (ret sf-result))
  :documentation			; FDL
  "The hypotenuse function sqrt{x^2 + y^2}.")

(defmfun sinc (x)
  "gsl_sf_sinc_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "sinc(x) = sin(pi x) / (pi x)}")

(defmfun log-sin (x)
  "gsl_sf_complex_logsin_e"
  (((realpart x) :double) ((imagpart x) :double)
   (re-ret sf-result) (im-ret sf-result))
  :documentation			; FDL
  "This function computes the logarithm of the complex sine,
  \log(\sin(z_r + i z_i)) storing the real and imaginary parts in
  szr, szi."
  :return ((complex (val re-ret) (val im-ret))
	   (complex (err re-ret) (err im-ret))))

;;;;****************************************************************************
;;;; Hyperbolic Trigonometric Functions
;;;;****************************************************************************

(defmfun log-sinh (x)
  "gsl_sf_lnsinh_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "Logarithm of sinh function, special functions
  These routines compute log(\sinh(x)) for x > 0.")

(defmfun log-cosh (x)
  "gsl_sf_lncosh_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "Logarithm of cosh function, special functions
   These routines compute log(cosh(x)) for any x.")

;;;;****************************************************************************
;;;; Conversion Functions
;;;;****************************************************************************

(defmfun polar-to-rectangular (r theta)
  "gsl_sf_polar_to_rect"
  ((r :double) (theta :double) (x sf-result) (y sf-result))
  :return ((val x) (val y) (err x) (err y))
  :documentation			; FDL
  "Convert the polar coordinates (r, theta) to
  rectilinear coordinates (x, y), x = r\cos(\theta), y = r\sin(\theta).")

(defmfun rectangular-to-polar (x y)
  "gsl_sf_rect_to_polar"
  ((x :double) (y :double) (r sf-result) (theta sf-result))
  :return ((val r) (val theta) (err r) (err theta))
  :documentation			; FDL
  "Convert the rectilinear coordinates (x, y) to
  polar coordinates (r, theta), such that x =
  r cos(theta)}, y = r sin(theta).  The argument theta
  lies in the range [-\pi, \pi].")

;;;;****************************************************************************
;;;; Restriction Functions
;;;;****************************************************************************

(defmfun restrict-symmetric (theta)
  "gsl_sf_angle_restrict_symm" ((theta :double))
  :c-return :double
  :documentation			; FDL
  "Force the angle theta to lie in the range (-\pi,\pi].")

(defmfun restrict-positive (theta)
  "gsl_sf_angle_restrict_pos" ((theta :double))
  :c-return :double
  :documentation			; FDL
  "Force the angle theta to lie in the range [0,2\pi).")

;;;;****************************************************************************
;;;; Trigonometric Functions With Error Estimates
;;;;****************************************************************************

(defmfun sin-err (x dx)
  "gsl_sf_sin_err_e" ((x :double) (dx :double) (ret sf-result))
  :documentation			; FDL
  "Compute the sine of an angle x with
   an associated absolute error dx, sin(x \pm dx).")

(defmfun cos-err (x dx)
  "gsl_sf_cos_err_e" ((x :double) (dx :double) (ret sf-result))
  :documentation			; FDL
  "The cosine of an angle x with an associated
  absolute error dx, cos(x \pm dx).")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests trigonometry
  (gsl-sin 1.0d0)
  (gsl-sin #C(1.0d0 1.0d0))
  (gsl-cos 1.0d0)
  (gsl-cos #C(1.0d0 1.0d0))
  (hypotenuse 1.0d0 2.0d0)
  (sinc 0.5d0)
  (log-sin #C(1.0d0 1.0d0))
  (log-sinh 0.5d0)
  (log-cosh 0.5d0)
  (polar-to-rectangular 2.0d0 1.0d0)
  (rectangular-to-polar 2.0d0 1.0d0)
  (restrict-symmetric 5.0d0)
  (restrict-positive -1.0d0)
  (sin-err 0.5d0 0.01d0)
  (cos-err 0.5d0 0.01d0))
|#

(LISP-UNIT:DEFINE-TEST TRIGONOMETRY
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8414709848078965d0 3.736881847550928d-16)
   (MULTIPLE-VALUE-LIST (GSL-SIN 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #C(1.2984575814159773d0 0.6349639147847361d0)
	 #C(5.766310013548447d-16 2.8198062320005596d-16))
   (MULTIPLE-VALUE-LIST (GSL-SIN #C(1.0d0 1.0d0))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5403023058681398d0 2.3994242409314904d-16)
   (MULTIPLE-VALUE-LIST (GSL-COS 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #C(0.8337300251311491d0 -0.9888977057628651d0)
	 #C(3.7025050808876487d-16 4.3915880077477046d-16))
   (MULTIPLE-VALUE-LIST (GSL-COS #C(1.0d0 1.0d0))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.23606797749979d0 9.930136612989092d-16)
   (MULTIPLE-VALUE-LIST (HYPOTENUSE 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6366197723675814d0 3.0072729231305663d-16)
   (MULTIPLE-VALUE-LIST (SINC 0.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST #C(0.3683837314249251d0 0.4548202333099499d0)
	 #C(1.6359524021011266d-16 8.179762010505633d-17))
   (MULTIPLE-VALUE-LIST (LOG-SIN #C(1.0d0 1.0d0))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -0.6518223259470272d0 2.8946726169244526d-16)
   (MULTIPLE-VALUE-LIST (LOG-SINH 0.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.12011450695827751d0 4.401938023864669d-17)
   (MULTIPLE-VALUE-LIST (LOG-COSH 0.5d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0806046117362795d0 1.682941969615793d0
	 8.535730329413909d-16 9.873187936033346d-16)
   (MULTIPLE-VALUE-LIST
    (POLAR-TO-RECTANGULAR 2.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 2.23606797749979d0 0.4636476090008061d0
	 9.930136612989092d-16 2.0590090033003876d-16)
   (MULTIPLE-VALUE-LIST
    (RECTANGULAR-TO-POLAR 2.0d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -1.2831853071795862d0)
   (MULTIPLE-VALUE-LIST (RESTRICT-SYMMETRIC 5.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 5.283185307179586d0)
   (MULTIPLE-VALUE-LIST (RESTRICT-POSITIVE -1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.479425538604203d0 0.008775825618904047d0)
   (MULTIPLE-VALUE-LIST (SIN-ERR 0.5d0 0.01d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8775825618903728d0 0.004794255386042614d0)
   (MULTIPLE-VALUE-LIST (COS-ERR 0.5d0 0.01d0))))
