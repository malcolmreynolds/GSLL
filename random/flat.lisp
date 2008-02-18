;; Flat distribution
;; Liam Healy, Oct  7 2006
;; Time-stamp: <2008-02-17 13:08:10EST flat.lisp>
;; $Id: $

(in-package :gsl)

(defmfun flat (generator a b)
  "gsl_ran_flat"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the flat (uniform)
   distribution from a to b.  The distribution is
   p(x) dx = {1 \over (b-a)} dx
   if a <= x < b, and 0 otherwise.")

(defmfun flat-pdf (x a b)
  "gsl_ran_flat_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a uniform distribution from a to b, using the formula
   given for #'flat.")

(defmfun flat-P (x a b)
  "gsl_cdf_flat_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   P(x) for a uniform distribution from a to b.")

(defmfun flat-Q (x a b)
  "gsl_cdf_flat_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(x) for a uniform distribution from a to b.")

(defmfun flat-Pinv (P a b)
  "gsl_cdf_flat_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   P(x) for a uniform distribution from a to b.")

(defmfun flat-Qinv (Q a b)
  "gsl_cdf_flat_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for a uniform distribution from a to b.")

;;; Examples and unit test
#|
(make-tests flat
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (flat rng 1.0d0 2.0d0)))
  (flat-pdf 1.2d0 1.0d0 2.0d0)
  (flat-P 1.2d0 1.0d0 2.0d0)
  (flat-Q 1.2d0 1.0d0 2.0d0)
  (flat-Pinv 0.19999999999999996d0 1.0d0 2.0d0)
  (flat-Qinv 0.8d0 1.0d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST FLAT
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 1.999741748906672d0 1.1629098753910512d0
	  1.2826178052928299d0 1.9472010820172727d0
	  1.2316565427463502d0 1.4849736143369228d0
	  1.9574769565369934d0 1.7443053431343287d0
	  1.540043658344075d0 1.7399529814720154d0
	  1.7599437981843948d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (FLAT RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0d0)
   (MULTIPLE-VALUE-LIST (FLAT-PDF 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.19999999999999996d0)
   (MULTIPLE-VALUE-LIST (FLAT-P 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8d0)
   (MULTIPLE-VALUE-LIST (FLAT-Q 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.2d0)
   (MULTIPLE-VALUE-LIST
    (FLAT-PINV 0.19999999999999996d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.2d0)
   (MULTIPLE-VALUE-LIST
    (FLAT-QINV 0.8d0 1.0d0 2.0d0))))

