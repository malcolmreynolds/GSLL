;; Weibull distribution
;; Liam Healy, Sun Oct 22 2006
;; Time-stamp: <2008-02-17 13:30:33EST weibull.lisp>
;; $Id$

(in-package :gsl)

(defmfun weibull (generator a b)
  "gsl_ran_weibull"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Weibull distribution.  The distribution function is
   p(x) dx = {b \over a^b} x^{b-1}  \exp(-(x/a)^b) dx
   for x >= 0.")

(defmfun weibull-pdf (x a b)
  "gsl_ran_weibull_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Weibull distribution with scale a and exponent b,
   using the formula given in #'weibull.")

(defmfun weibull-P (x a b)
  "gsl_cdf_weibull_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   P(x) for the Weibull distribution with scale a and exponent b.")

(defmfun weibull-Q (x a b)
  "gsl_cdf_weibull_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the Weibull distribution with scale a and exponent b.")

(defmfun weibull-Pinv (P a b)
  "gsl_cdf_weibull_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the Weibull distribution scale a and exponent b.")

(defmfun weibull-Qinv (Q a b)
  "gsl_cdf_weibull_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the Weibull distribution exponent a and scale b.")

;;; Examples and unit test
#|
(make-tests weibull
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (weibull rng 1.0d0 2.0d0)))
  (weibull-pdf 1.5d0 1.3d0 1.0d0)
  (weibull-P 3.5d0 1.3d0 2.0d0)
  (weibull-Q 3.5d0 1.3d0 2.0d0)
  (weibull-Pinv 0.9992887742799077d0 1.3d0 2.0d0)
  (weibull-Qinv 7.112257200923508d-4 1.3d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST WEIBULL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.0160712303786595d0 1.347055359960668d0
	  1.1241262408795445d0 0.2329031397826649d0
	  1.209338423856536d0 0.8506825453443836d0
	  0.20845532973957762d0 0.5434187344070215d0
	  0.7849237503774361d0 0.5487883320132739d0
	  0.5239377808419179d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (WEIBULL RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.24263174972226745d0)
   (MULTIPLE-VALUE-LIST (WEIBULL-PDF 1.5d0 1.3d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9992887742799077d0)
   (MULTIPLE-VALUE-LIST (WEIBULL-P 3.5d0 1.3d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 7.112257200923508d-4)
   (MULTIPLE-VALUE-LIST (WEIBULL-Q 3.5d0 1.3d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 3.5000000000000164d0)
   (MULTIPLE-VALUE-LIST
    (WEIBULL-PINV 0.9992887742799077d0 1.3d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 3.5d0)
   (MULTIPLE-VALUE-LIST (WEIBULL-QINV 7.112257200923508d-4 1.3d0 2.0d0))))
