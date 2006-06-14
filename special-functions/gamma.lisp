;********************************************************
; file:        gamma.lisp                                
; description: Gamma functions                           
; date:        Thu Apr 27 2006 - 22:06                   
; author:      Liam M. Healy                             
; modified:    Tue Jun 13 2006 - 22:16
;********************************************************
;;; $Id: $

(in-package :gsl)

;;; Need to handle incoming gsl-complex numbers correctly for log-gamma-complex.
;;; Should functions returning sf-result and something else return the
;;; error at the end?

;;;;****************************************************************************
;;;; Gamma functions
;;;;****************************************************************************

(defconstant +gamma-xmax+ 171.0d0)

(defun-gsl gamma (x)
  "gsl_sf_gamma_e" ((x :double) (ret sf-result))
  :documentation "The Gamma function @math{\Gamma(x)}, subject to x
   not being a negative integer.  The function is computed using the real
   Lanczos method. The maximum value of @math{x} such that
   @math{\Gamma(x)} is not considered an overflow is given by +gamma-xmax+.")

(defun-gsl log-gamma (x)
  "gsl_sf_lngamma_e" ((x :double) (ret sf-result))
  :documentation "The logarithm of the Gamma function,
   @math{\log(\Gamma(x))}, subject to @math{x} not a being negative
   integer.  For @math{x<0} the real part of @math{\log(\Gamma(x))} is
   returned, which is equivalent to @math{\log(|\Gamma(x)|)}.  The function
   is computed using the real Lanczos method.")

(defun-gsl log-gamma-sign (x)
  "gsl_sf_lngamma_sgn_e" ((x :double) (ret sf-result) (sign :double))
  :documentation "Compute the sign of the gamma function and the logarithm of
  its magnitude, subject to @math{x} not being a negative integer.  The
  function is computed using the real Lanczos method.  The value of the
  gamma function can be reconstructed using the relation @math{\Gamma(x) =
  sgn * \exp(resultlg)}."
  :return ((val ret) (double-to-cl sign) (err ret)))

(defun-gsl gamma* (x)
  "gsl_sf_gammastar_e" ((x :double) (ret sf-result))
  :Documentation "The regulated Gamma Function @math{\Gamma^*(x)}
  for @math{x > 0}, given by
  \Gamma^*(x) &= \Gamma(x)/(\sqrt{2\pi} x^{(x-1/2)} \exp(-x))\cr
            &= \left(1 + {1 \over 12x} + ...\right)
  \quad\hbox{for~} x\to \infty\cr.")

(defun-gsl 1/gamma (x)
  "gsl_sf_gammainv_e" ((x :double) (ret sf-result))
  :documentation "The reciprocal of the gamma function,
  @math{1/\Gamma(x)} using the real Lanczos method.")

(defun-gsl log-gamma-complex (z)
  "gsl_sf_lngamma_complex_e"
  (((realpart z) :double) ((imagpart z) :double)
   (lnr sf-result) (arg sf-result))
  :documentation "Compute @math{\log(\Gamma(z))} for complex @math{z=z_r+i
  z_i} and @math{z} not a negative integer, using the complex Lanczos
  method.  The returned parameters are @math{lnr = \log|\Gamma(z)|} and
  @math{arg = \arg(\Gamma(z))} in @math{(-\pi,\pi]}.  Note that the phase
  part (@var{arg}) is not well-determined when @math{|z|} is very large,
  due to inevitable roundoff in restricting to @math{(-\pi,\pi]}.  This
  will result in a @code{GSL_ELOSS} error when it occurs.  The absolute
  value part (@var{lnr}), however, never suffers from loss of precision."
  :return
  ((val lnr) (val arg) (err lnr) (err arg)))

(defun-gsl taylor-coefficient (n x)
  "gsl_sf_taylorcoeff_e" ((n :int) (x :double) (ret sf-result))
  :documentatiOn "Compute the Taylor coefficient @math{x^n / n!} for 
  @math{x >= 0}, @math{n >= 0}.")

(defun-gsl factorial (n)
  "gsl_sf_fact_e" ((n :size) (ret sf-result))
  :documentation "The factorial @math{n!},
  related to the Gamma function by @math{n! = \Gamma(n+1)}.")

(defun-gsl double-factorial (n)
  "gsl_sf_doublefact_e" ((n :size) (ret sf-result))
  :documentation "The double factorial @math{n!! = n(n-2)(n-4) \dots}.")

(defun-gsl log-factorial (n)
  "gsl_sf_lnfact_e" ((n :size) (ret sf-result))
  :documentation "The logarithm of the factorial of @var{n},
  @math{\log(n!)}.  The algorithm is faster than computing
  @math{\ln(\Gamma(n+1))} via @code{gsl_sf_lngamma} for @math{n < 170},
  but defers for larger @var{n}.")

(defun-gsl log-double-factorial (n)
  "gsl_sf_lndoublefact_e" ((n :size) (ret sf-result))
  :documentation "These routines compute the logarithm of
  the double factorial of @var{n}, @math{\log(n!!)}.")

(defun-gsl choose (n m)
  "gsl_sf_choose_e" ((n :size) (m :size) (ret sf-result))
  :documentation "The combinatorial factor @code{n choose m}
  @math{= n!/(m!(n-m)!)}")

(defun-gsl log-choose (n m)
  "gsl_sf_lnchoose_e" ((n :size) (m :size) (ret sf-result))
  :documentation "The logarithm of @code{n choose m}.  This is
  equivalent to the sum @math{\log(n!) - \log(m!) - \log((n-m)!)}.")

(defun-gsl pochammer (a x)
  "gsl_sf_poch_e"  ((a :double) (x :double) (ret sf-result))
  :documentation "The Pochhammer symbol @math{(a)_x := \Gamma(a +
   x)/\Gamma(a)}, subject to @math{a} and @math{a+x} not being negative
   integers. The Pochhammer symbol is also known as the Apell symbol and
   sometimes written as @math{(a,x)}.")

(defun-gsl log-pochammer (a x)
  "gsl_sf_lnpoch_e" ((a :double) (x :double) (ret sf-result))
  :documentation "The logarithm of the Pochhammer symbol,
  @math{\log((a)_x) = \log(\Gamma(a + x)/\Gamma(a))} for @math{a > 0},
  @math{a+x > 0}.")

(defun-gsl log-pochammer-sign (a x)
  "gsl_sf_lnpoch_sgn_e"
  ((a :double) (x :double) (ret sf-result) (sign :double))
  :documentation "The logarithm of the Pochhammer symbol and its sign.
  The computed parameters are @math{result =
  \log(|(a)_x|)} and @math{sgn = \sgn((a)_x)} where @math{(a)_x :=
  \Gamma(a + x)/\Gamma(a)}, subject to @math{a}, @math{a+x} not being
  negative integers."
  :return ((val ret) (double-to-cl sign) (err ret)))

(defun-gsl relative-pochammer (a x)
  "gsl_sf_pochrel_e" ((a :double) (x :double) (ret sf-result))
  :documentation "The relative Pochhammer symbol @math{((a)_x -
  1)/x} where @math{(a)_x := \Gamma(a + x)/\Gamma(a)}.")

(defun-gsl incomplete-gamma (a x)
  "gsl_sf_gamma_inc_Q_e" ((a :double) (x :double) (ret sf-result))
  :documentation "The normalized incomplete Gamma Function
  @math{Q(a,x) = 1/\Gamma(a) \int_x^\infty dt t^@{a-1@} \exp(-t)}
  for @math{a > 0}, @math{x >= 0}.")

(defun-gsl complementary-incomplete-gamma (a x)
  "gsl_sf_gamma_inc_P_e" ((a :double) (x :double) (ret sf-result))
  :documentation "The complementary normalized incomplete Gamma Function
  @math{P(a,x) = 1/\Gamma(a) \int_0^x dt t^@{a-1@} \exp(-t)}
  for @math{a > 0}, @math{x >= 0}.  Note that Abramowitz & Stegun
  call @math{P(a,x)} the incomplete gamma function (section 6.5).")

(defun-gsl nonnormalized-incomplete-gamma (a x)
  "gsl_sf_gamma_inc_e" ((a :double) (x :double) (ret sf-result))
  :documentation "The incomplete Gamma Function
   @math{\Gamma(a,x)}, without the normalization factor
   included in the previously defined functions:
   @math{\Gamma(a,x) = \int_x^\infty dt t^@{a-1@} \exp(-t)}
   for @math{a} real and @math{x >= 0}.")

(defun-gsl beta (a b)
  "gsl_sf_beta_e" ((a :double) (b :double) (ret sf-result))
  :documentation "The Beta Function, @math{B(a,b) =
  \Gamma(a)\Gamma(b)/\Gamma(a+b)} for @math{a > 0}, @math{b > 0}.")

(defun-gsl log-beta (a b)
  "gsl_sf_lnbeta_e" ((a :double) (b :double) (ret sf-result))
  :documentation "The logarithm of the Beta Function,
  @math{\log(B(a,b))} for @math{a > 0}, @math{b > 0}.")

(defun-gsl incomplete-beta (a b x)
  "gsl_sf_beta_inc_e"
  ((a :double) (b :double) (x :double) (ret sf-result))
  :documentation "The normalized incomplete Beta function
   @math{B_x(a,b)/B(a,b)} where
   @math{B_x(a,b) = \int_0^x t^@{a-1@} (1-t)^@{b-1@} dt}
   for @math{a > 0}, @math{b > 0}, and @math{0 <= x <= 1}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test gamma
  (lisp-unit:assert-error 'gsl-error (gamma -1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000d+03"
   (gamma 6.0d0))
  (lisp-unit:assert-error 'gsl-error (log-gamma -100.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.359134205370d+03"
   (log-gamma 100.0d0))
  (lisp-unit:assert-equal
   '("0.359134205370d+03" "0.100000000000d+01")
   (subseq (lisp-unit:fp-values (log-gamma-sign 100.0d0)) 0 2))
  (lisp-unit:assert-first-fp-equal
   "0.100347805583d+01"
   (gamma* 24.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.198412698413d-03"
   (1/gamma 8.0d0))
  (lisp-unit:assert-equal
   '("0.823613175045d+01" "-0.118403781494d+01")
   (subseq
    (lisp-unit:fp-values (log-gamma-complex #C(10.0d0 10.0d0)))
    0 2))
  (lisp-unit:assert-first-fp-equal
   "0.110947646104d-02"
   (taylor-coefficient 12 3.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.479001600000d+09"
   (factorial 12))
  (lisp-unit:assert-first-fp-equal
   "0.460800000000d+05"
   (double-factorial 12))
  (lisp-unit:assert-first-fp-equal
   "0.857933669826d+03"
   (log-factorial 199))
  (lisp-unit:assert-first-fp-equal
   "0.430177893581d+03"
   (log-double-factorial 199))
  (lisp-unit:assert-first-fp-equal
   "0.560000000000d+02"
   (choose 8 3))
  (lisp-unit:assert-error 'gsl-error (choose 3 8))
  (lisp-unit:assert-first-fp-equal
   "0.294222741699d+02"
   (log-choose 67 12))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000d+02"
   (pochammer 3.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.863231987192d+03"
   (log-pochammer 2.0d0 199.0d0))
  (lisp-unit:assert-equal
   '("0.863231987192d+03" "0.100000000000d+01")
   (subseq (lisp-unit:fp-values
	    (log-pochammer-sign 2.0d0 199.0d0))
	   0 2))
  (lisp-unit:assert-first-fp-equal
   "0.403199888889d+06"
   (relative-pochammer 2.0d0 9.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.406005849710d+00"
   (incomplete-gamma 2.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.593994150290d+00"
   (complementary-incomplete-gamma 2.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.406005849710d+00"
   (nonnormalized-incomplete-gamma 2.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.181818181818d+00"
   (beta 5.50d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "-0.170474809224d+01"
   (log-beta 5.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.646446609407d+00"
   (incomplete-beta 1.0d0 1.50d0 0.50d0)))
