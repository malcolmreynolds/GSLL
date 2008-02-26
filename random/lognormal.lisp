;; Lognormal distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 13:10:13EST lognormal.lisp>
;; $Id$

(in-package :gsl)

(defmfun lognormal (generator zeta sigma)
  "gsl_ran_lognormal"
  (((generator generator) :pointer) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the lognormal distribution.
   The distribution function is
   p(x) dx = {1 \over x \sqrt{2 \pi \sigma^2}} \exp(-(\ln(x) - \zeta)^2/2 \sigma^2) dx
   for x > 0.")

(defmfun lognormal-pdf (x zeta sigma)
  "gsl_ran_lognormal_pdf" ((x :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at X
   for a lognormal distribution with parameters zeta and sigma,
   using the formula given in #'lognormal.")

(defmfun lognormal-P (x zeta sigma)
  "gsl_cdf_lognormal_P" ((x :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the lognormal distribution with parameters zeta and sigma.")

(defmfun lognormal-Q (x zeta sigma)
  "gsl_cdf_lognormal_Q" ((x :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the lognormal distribution with parameters
  zeta and sigma.")

(defmfun lognormal-Pinv (P zeta sigma)
  "gsl_cdf_lognormal_Pinv" ((P :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the lognormal distribution with parameters
  zeta and sigma.")

(defmfun lognormal-Qinv (Q zeta sigma)
  "gsl_cdf_lognormal_Qinv" ((Q :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the lognormal distribution with parameters
   zeta and sigma.")

;;; Examples and unit test
#|
(make-tests lognormal
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (lognormal rng 1.0d0 2.0d0)))
  (lognormal-pdf 1.2d0 1.0d0 2.0d0)
  (lognormal-P 1.2d0 1.0d0 2.0d0)
  (lognormal-Q 1.2d0 1.0d0 2.0d0)
  (lognormal-Pinv 0.3413288272347352d0 1.0d0 2.0d0)
  (lognormal-Qinv 0.6586711727652649d0 1.0d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST LOGNORMAL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 2.386447068127768d0 0.11687602167359055d0
	  4.753374578796263d0 30.093393775755004d0
	  0.8119584375760986d0 3.163421055157545d0
	  0.9146206567715651d0 0.727307901065758d0
	  2.180184852178898d0 3.8908856616896017d0
	  182.18469788916977d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (LOGNORMAL RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.15289833965691607d0)
   (MULTIPLE-VALUE-LIST
    (LOGNORMAL-PDF 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.3413288272347351d0)
   (MULTIPLE-VALUE-LIST (LOGNORMAL-P 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6586711727652649d0)
   (MULTIPLE-VALUE-LIST (LOGNORMAL-Q 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.2000000000000004d0)
   (MULTIPLE-VALUE-LIST
    (LOGNORMAL-PINV 0.3413288272347352d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.2d0)
   (MULTIPLE-VALUE-LIST
    (LOGNORMAL-QINV 0.6586711727652649d0 1.0d0 2.0d0))))
