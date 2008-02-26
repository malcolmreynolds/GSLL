;; Tdist distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2008-02-17 13:16:23EST tdist.lisp>
;; $Id$

(in-package :gsl)

(defmfun tdist (generator nu)
  "gsl_ran_tdist"
  (((generator generator) :pointer) (nu :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Student t-distribution.  The
   distribution function is,
   p(x) dx = {\Gamma((\nu + 1)/2) \over \sqrt{\pi \nu} \Gamma(\nu/2)}
   (1 + x^2/\nu)^{-(\nu + 1)/2} dx
   for -\infty < x < +\infty.")

(defmfun tdist-pdf (x nu)
  "gsl_ran_tdist_pdf" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a t-distribution with nu degrees of freedom, using the formula
   given in #'tdist.")

(defmfun tdist-P (x nu)
  "gsl_cdf_tdist_P" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the tdist distribution with nu degrees of freedom.")

(defmfun tdist-Q (x nu)
  "gsl_cdf_tdist_Q" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(x) for the tdist distribution with nu degrees of freedom.")

(defmfun tdist-Pinv (P nu)
  "gsl_cdf_tdist_Pinv" ((P :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   P(x) for the tdist distribution with nu degrees of freedom.")

(defmfun tdist-Qinv (Q nu)
  "gsl_cdf_tdist_Qinv" ((Q :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the tdist distribution with nu degrees of freedom.")

;;; Examples and unit test
#|
(make-tests tdist
  (letm ((rng (random-number-generator *mt19937* 0)))
      (rng-set rng 0)
      (loop for i from 0 to 10
	    collect
	    (tdist rng 10.0d0)))
  (tdist-pdf 0.5d0 1.0d0)
  (tdist-P 0.5d0 1.0d0)
  (tdist-Q 0.5d0 1.0d0)
  (tdist-Pinv 0.6475836176504334d0 1.0d0)
  (tdist-Qinv 0.3524163823495667d0 1.0d0))
|#

(LISP-UNIT:DEFINE-TEST TDIST
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.14989366374481017d0 0.6794142879291215d0
	  -1.615833951108472d0 -1.6008862825783456d0
	  -1.7010935505767397d0 -0.04370959749808691d0
	  0.12761159276595174d0 -0.019731218255494867d0
	  -0.6534666117199732d0 0.2035771324523077d0
	  1.77650300477611d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (RNG-SET RNG 0)
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (TDIST RNG 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.2546479089470325d0)
   (MULTIPLE-VALUE-LIST (TDIST-PDF 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6475836176504332d0)
   (MULTIPLE-VALUE-LIST (TDIST-P 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.35241638234956685d0)
   (MULTIPLE-VALUE-LIST (TDIST-Q 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.500000000000001d0)
   (MULTIPLE-VALUE-LIST
    (TDIST-PINV 0.6475836176504334d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5d0)
   (MULTIPLE-VALUE-LIST (TDIST-QINV 0.3524163823495667d0 1.0d0))))
