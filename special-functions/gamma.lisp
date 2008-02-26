;; Gamma functions
;; Liam Healy, Thu Apr 27 2006 - 22:06
;; Time-stamp: <2008-02-16 21:58:18EST gamma.lisp>
;; $Id$

(in-package :gsl)

;;; Need to handle incoming gsl-complex numbers correctly for log-gamma-complex.
;;; Should functions returning sf-result and something else return the
;;; error at the end?

;;;;****************************************************************************
;;;; Gamma functions
;;;;****************************************************************************

(defconstant +gamma-xmax+ 171.0d0)

(defmfun gamma (x)
  "gsl_sf_gamma_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The Gamma function Gamma(x), subject to x
   not being a negative integer.  The function is computed using the real
   Lanczos method. The maximum value of x such that
   Gamma(x) is not considered an overflow is given by +gamma-xmax+.")

(defmfun log-gamma (x)
  "gsl_sf_lngamma_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The logarithm of the Gamma function,
   log(Gamma(x)), subject to x not a being negative
   integer.  For x<0 the real part of log(Gamma(x)) is
   returned, which is equivalent to log(|Gamma(x)|).  The function
   is computed using the real Lanczos method.")

(defmfun log-gamma-sign (x)
  "gsl_sf_lngamma_sgn_e" ((x :double) (ret sf-result) (sign :double))
  :return ((val ret) (dcref sign) (err ret))
  :documentation			; FDL
  "Compute the sign of the gamma function and the logarithm of
  its magnitude, subject to x not being a negative integer.  The
  function is computed using the real Lanczos method.  The value of the
  gamma function can be reconstructed using the relation Gamma(x) =
  sgn * exp(resultlg)}.")

(defmfun gamma* (x)
  "gsl_sf_gammastar_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The regulated Gamma Function Gamma^*(x)
  for x > 0, given by
  Gamma^*(x) = Gamma(x)/(sqrt{2\pi} x^{(x-1/2)} \exp(-x))
            = (1 + {1 \over 12x} + ...)
  for x to infinity.")

(defmfun 1/gamma (x)
  "gsl_sf_gammainv_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The reciprocal of the gamma function,
  1/\Gamma(x) using the real Lanczos method.")

(defmfun log-gamma-complex (z)
  "gsl_sf_lngamma_complex_e"
  (((realpart z) :double) ((imagpart z) :double)
   (lnr sf-result) (arg sf-result))
  :documentation			; FDL
  "Compute log(Gamma(z)) for complex z=z_r+i z_i
  and z not a negative integer, using the complex Lanczos
  method.  The returned parameters are lnr = log|\Gamma(z)| and
  arg = arg(Gamma(z)) in (-pi,pi].  Note that the phase
  part (arg) is not well-determined when |z| is very large,
  due to inevitable roundoff in restricting to (-\pi,\pi].  This
  will result in a :ELOSS error when it occurs.  The absolute
  value part (lnr), however, never suffers from loss of precision."
  :return
  ((val lnr) (val arg) (err lnr) (err arg)))

(defmfun taylor-coefficient (n x)
  "gsl_sf_taylorcoeff_e" ((n :int) (x :double) (ret sf-result))
  :documentation			; FDL
  "Compute the Taylor coefficient x^n / n! for x >= 0, n >= 0.")

(defmfun factorial (n)
  "gsl_sf_fact_e" ((n size) (ret sf-result))
  :documentation			; FDL
  "The factorial n!, related to the Gamma function by n! = \Gamma(n+1).")

(defmfun double-factorial (n)
  "gsl_sf_doublefact_e" ((n size) (ret sf-result))
  :documentation			; FDL
  "The double factorial n!! = n(n-2)(n-4) \dots.")

(defmfun log-factorial (n)
  "gsl_sf_lnfact_e" ((n size) (ret sf-result))
  :documentation			; FDL
  "The logarithm of the factorial of n, log(n!).
  The algorithm is faster than computing
  ln(Gamma(n+1)) via #'log-gamma for n < 170, but defers for larger n.")

(defmfun log-double-factorial (n)
  "gsl_sf_lndoublefact_e" ((n size) (ret sf-result))
  :documentation			; FDL
  "Compute the logarithm of the double factorial of n, log(n!!).")

(defmfun choose (n m)
  "gsl_sf_choose_e" ((n size) (m size) (ret sf-result))
  :documentation			; FDL
  "The combinatorial factor (n choose m) = n!/(m!(n-m)!).")

(defmfun log-choose (n m)
  "gsl_sf_lnchoose_e" ((n size) (m size) (ret sf-result))
  :documentation			; FDL
  "The logarithm of (n choose m).  This is
  equivalent to the sum log(n!) - log(m!) - log((n-m)!).")

(defmfun pochammer (a x)
  "gsl_sf_poch_e"  ((a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The Pochhammer symbol (a)_x := Gamma(a +
   x)/Gamma(a), subject to a and a+x not being negative
   integers. The Pochhammer symbol is also known as the Apell symbol and
   sometimes written as (a,x).")

(defmfun log-pochammer (a x)
  "gsl_sf_lnpoch_e" ((a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The logarithm of the Pochhammer symbol,
  log((a)_x) = log(Gamma(a + x)/Gamma(a)) for a > 0, a+x > 0.")

(defmfun log-pochammer-sign (a x)
  "gsl_sf_lnpoch_sgn_e"
  ((a :double) (x :double) (ret sf-result) (sign :double))
  :documentation			; FDL
  "The logarithm of the Pochhammer symbol and its sign.
  The computed parameters are result =
  log(|(a)_x|) and sgn = sgn((a)_x) where (a)_x :=
  Gamma(a + x)/Gamma(a), subject to a, a+x not being negative integers."
  :return ((val ret) (dcref sign) (err ret)))

(defmfun relative-pochammer (a x)
  "gsl_sf_pochrel_e" ((a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The relative Pochhammer symbol ((a)_x -
  1)/x where (a)_x := Gamma(a + x)/Gamma(a)}.")

(defmfun incomplete-gamma (a x)
  "gsl_sf_gamma_inc_Q_e" ((a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The normalized incomplete Gamma Function
  Q(a,x) = 1/Gamma(a) \int_x^\infty dt t^{a-1} \exp(-t) for a > 0, x >= 0.")

(defmfun complementary-incomplete-gamma (a x)
  "gsl_sf_gamma_inc_P_e" ((a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The complementary normalized incomplete Gamma Function
  P(a,x) = 1/\Gamma(a) \int_0^x dt t^{a-1} \exp(-t)}
  for a > 0, x >= 0.  Note that Abramowitz & Stegun
  call P(a,x) the incomplete gamma function (section 6.5).")

(defmfun nonnormalized-incomplete-gamma (a x)
  "gsl_sf_gamma_inc_e" ((a :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The incomplete Gamma Function
   Gamma(a,x), without the normalization factor
   included in the previously defined functions:
   Gamma(a,x) = \int_x^\infty dt t^{a-1} \exp(-t)
   for a real and x >= 0.")

(defmfun beta (a b)
  "gsl_sf_beta_e" ((a :double) (b :double) (ret sf-result))
  :documentation			; FDL
  "The Beta Function, B(a,b) = Gamma(a)Gamma(b)/Gamma(a+b)} for a > 0, b > 0.")

(defmfun log-beta (a b)
  "gsl_sf_lnbeta_e" ((a :double) (b :double) (ret sf-result))
  :documentation			; FDL
  "The logarithm of the Beta Function, log(B(a,b)) for a > 0, b > 0.")

(defmfun incomplete-beta (a b x)
  "gsl_sf_beta_inc_e"
  ((a :double) (b :double) (x :double) (ret sf-result))
  :documentation			; FDL
  "The normalized incomplete Beta function
   B_x(a,b)/B(a,b) where
   B_x(a,b) = \int_0^x t^{a-1} (1-t)^{b-1} dt
   for a > 0, b > 0, and 0 <= x <= 1.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests gamma
  (gamma -1.0d0)
  (gamma 6.0d0)
  (log-gamma -100.0d0)
  (log-gamma 100.0d0)
  (log-gamma-sign 100.0d0)
  (gamma* 24.0d0)
  (1/gamma 8.0d0)
  (log-gamma-complex #C(10.0d0 10.0d0))
  (taylor-coefficient 12 3.0d0)
  (factorial 12)
  (double-factorial 12)
  (log-factorial 199)
  (log-double-factorial 199)
  (choose 8 3)
  (choose 3 8)
  (log-choose 67 12)
  (pochammer 3.0d0 2.0d0)
  (log-pochammer 2.0d0 199.0d0)
  (log-pochammer-sign 2.0d0 199.0d0)
  (relative-pochammer 2.0d0 9.0d0)
  (incomplete-gamma 2.0d0 2.0d0)
  (complementary-incomplete-gamma 2.0d0 2.0d0)
  (nonnormalized-incomplete-gamma 2.0d0 2.0d0)
  (beta 5.50d0 1.0d0)
  (log-beta 5.5d0 1.0d0)
  (incomplete-beta 1.0d0 1.50d0 0.50d0))
|#

(LISP-UNIT:DEFINE-TEST GAMMA
  (LISP-UNIT:ASSERT-ERROR 'GSL-ERROR (GAMMA -1.0d0))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 120.0d0 2.6645352591003757d-14)
   (MULTIPLE-VALUE-LIST (GAMMA 6.0d0)))
  (LISP-UNIT:ASSERT-ERROR 'GSL-ERROR (LOG-GAMMA -100.0d0))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 359.13420536957534d0 2.4544868717695813d-13)
   (MULTIPLE-VALUE-LIST (LOG-GAMMA 100.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 359.13420536957534d0 1.0d0
	 2.4544868717695813d-13)
   (MULTIPLE-VALUE-LIST (LOG-GAMMA-SIGN 100.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0034780558311105d0 4.456337769159149d-16)
   (MULTIPLE-VALUE-LIST (GAMMA* 24.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.984126984126984d-4 1.3216940769347103d-19)
   (MULTIPLE-VALUE-LIST (1/GAMMA 8.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 8.236131750448724d0 -1.1840378149363078d0
	 7.315154482555574d-15 2.1796539969587586d-14)
   (MULTIPLE-VALUE-LIST
    (LOG-GAMMA-COMPLEX #C(10.0d0 10.0d0))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0011094764610389606d0 2.956239149580215d-18)
   (MULTIPLE-VALUE-LIST (TAYLOR-COEFFICIENT 12 3.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 4.790016d8 0.0d0)
   (MULTIPLE-VALUE-LIST (FACTORIAL 12)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 46080.0d0 0.0d0)
   (MULTIPLE-VALUE-LIST (DOUBLE-FACTORIAL 12)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 857.9336698258575d0 5.777158772429952d-13)
   (MULTIPLE-VALUE-LIST (LOG-FACTORIAL 199)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 430.17789358084747d0 1.9103736085528287d-13)
   (MULTIPLE-VALUE-LIST (LOG-DOUBLE-FACTORIAL 199)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 56.0d0 7.460698725481052d-14)
   (MULTIPLE-VALUE-LIST (CHOOSE 8 3)))
  (LISP-UNIT:ASSERT-ERROR 'GSL-ERROR (CHOOSE 3 8))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 29.422274169864693d0 1.9338924605168215d-13)
   (MULTIPLE-VALUE-LIST (LOG-CHOOSE 67 12)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 12.0d0 6.927791673660977d-14)
   (MULTIPLE-VALUE-LIST (POCHAMMER 3.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 863.2319871924054d0 9.645972767118375d-13)
   (MULTIPLE-VALUE-LIST (LOG-POCHAMMER 2.0d0 199.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 863.2319871924054d0 1.0d0 9.645972767118375d-13)
   (MULTIPLE-VALUE-LIST
    (LOG-POCHAMMER-SIGN 2.0d0 199.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 403199.88888888917d0 3.5998302729212807d-9)
   (MULTIPLE-VALUE-LIST (RELATIVE-POCHAMMER 2.0d0 9.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.40600584970983766d0 9.568405127077496d-16)
   (MULTIPLE-VALUE-LIST (INCOMPLETE-GAMMA 2.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5939941502901611d0 3.510166705531295d-15)
   (MULTIPLE-VALUE-LIST
    (COMPLEMENTARY-INCOMPLETE-GAMMA 2.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.40600584970983766d0 1.2272947381959672d-15)
   (MULTIPLE-VALUE-LIST
    (NONNORMALIZED-INCOMPLETE-GAMMA 2.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.1818181818181818d0 1.0189782228738177d-15)
   (MULTIPLE-VALUE-LIST (BETA 5.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST -1.7047480922384253d0 3.81791013808375d-15)
   (MULTIPLE-VALUE-LIST (LOG-BETA 5.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6464466094067263d0 1.0178662453689468d-14)
   (MULTIPLE-VALUE-LIST
    (INCOMPLETE-BETA 1.0d0 1.5d0 0.5d0))))
