;; Cauchy distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 12:55:30EST cauchy.lisp>
;; $Id: $

(in-package :gsl)

(defmfun cauchy (generator a)
  "gsl_ran_cauchy"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Cauchy distribution with
   scale parameter a.  The probability distribution for Cauchy
   random variates is,
   p(x) dx = {1 \over a\pi (1 + (x/a)^2) } dx
   for x in the range -\infty to +\infty.  The Cauchy
   distribution is also known as the Lorentz distribution.")

(defmfun cauchy-pdf (x a)
  "gsl_ran_cauchy_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Cauchy distribution with scale parameter a, using the formula
   given for #'cauchy.")

(defmfun cauchy-P (x a)
  "gsl_cdf_cauchy_P" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the Cauchy distribution with scale parameter a.")

(defmfun cauchy-Q (x a)
  "gsl_cdf_cauchy_Q" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the Cauchy distribution with scale parameter a.")

(defmfun cauchy-Pinv (P a)
  "gsl_cdf_cauchy_Pinv" ((P :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the Cauchy distribution with scale parameter a.")

(defmfun cauchy-Qinv (Q a)
  "gsl_cdf_cauchy_Qinv" ((Q :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the Cauchy distribution with scale parameter a.")

;;; Examples and unit test
#|
(make-tests cauchy
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (cauchy rng 10.0d0)))
  (cauchy-pdf 0.0d0 10.0d0)
  (cauchy-P 1.0d0 2.0d0)
  (cauchy-Q 1.0d0 2.0d0)
  (cauchy-Pinv 0.6475836176504333d0 2.0d0)
  (cauchy-Qinv 0.35241638234956674d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST CAUCHY
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST -0.00811319915595434d0 5.617196410586812d0
	  12.292369828923075d0 -1.6741088357812182d0
	  8.909104486260928d0 211.6765861544609d0
	  -1.3439049184367153d0 -10.364363282910663d0
	  -79.0709314248171d0 -10.652071087998578d0
	  -9.393948243493877d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (CAUCHY RNG 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.03183098861837907d0)
   (MULTIPLE-VALUE-LIST (CAUCHY-PDF 0.0d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6475836176504333d0)
   (MULTIPLE-VALUE-LIST (CAUCHY-P 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.35241638234956674d0)
   (MULTIPLE-VALUE-LIST (CAUCHY-Q 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9999999999999998d0)
   (MULTIPLE-VALUE-LIST
    (CAUCHY-PINV 0.6475836176504333d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0000000000000002d0)
   (MULTIPLE-VALUE-LIST
    (CAUCHY-QINV 0.35241638234956674d0 2.0d0))))
