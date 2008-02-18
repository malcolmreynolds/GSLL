;; Rayleigh distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 18:45:41EST rayleigh.lisp>
;; $Id: $

(in-package :gsl)

(defmfun rayleigh (generator sigma)
  "gsl_ran_rayleigh"
  (((generator generator) :pointer) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Rayleigh distribution with
   scale parameter sigma.  The distribution is
   p(x) dx = {x \over \sigma^2} \exp(- x^2/(2 \sigma^2)) dx
   for x > 0.")

(defmfun rayleigh-pdf (x sigma)
  "gsl_ran_rayleigh_pdf" ((x :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Rayleigh distribution with scale parameter sigma, using the
   formula given for #'rayleigh.")

(defmfun rayleigh-P (x sigma)
  "gsl_cdf_rayleigh_P" ((x :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
  P(x) for the Rayleigh distribution with scale
  parameter sigma.")

(defmfun rayleigh-Q (x sigma)
  "gsl_cdf_rayleigh_Q" ((x :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
  Q(x) for the Rayleigh distribution with scale
  parameter sigma.")

(defmfun rayleigh-Pinv (P sigma)
  "gsl_cdf_rayleigh_Pinv" ((P :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function
  P(x)} for the Rayleigh distribution with scale
  parameter sigma.")

(defmfun rayleigh-Qinv (Q sigma)
  "gsl_cdf_rayleigh_Qinv" ((Q :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function
  Q(x) for the Rayleigh distribution with scale
  parameter sigma.")

;;; Examples and unit test
#|
(make-tests rayleigh
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (rayleigh rng 10.0d0)))
  (rayleigh-pdf 0.5d0 1.0d0)
  (rayleigh-P 1.0d0 2.0d0)
  (rayleigh-Q 1.0d0 2.0d0)
  (rayleigh-Pinv 0.1175030974154046d0 2.0d0)
  (rayleigh-Qinv 0.8824969025845955d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST RAYLEIGH
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.22728151965522753d0 19.05023959323748d0
	  15.897545756713367d0 3.2937477899992147d0
	  17.102628005168157d0 12.030467929000928d0
	  2.9480035446666624d0 7.6851014424603274d0
	  11.100498132125239d0 7.76103902005281d0
	  7.409599155063027d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (RAYLEIGH RNG 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4412484512922977d0)
   (MULTIPLE-VALUE-LIST (RAYLEIGH-PDF 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.1175030974154046d0)
   (MULTIPLE-VALUE-LIST (RAYLEIGH-P 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8824969025845955d0)
   (MULTIPLE-VALUE-LIST (RAYLEIGH-Q 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0000000000000002d0)
   (MULTIPLE-VALUE-LIST
    (RAYLEIGH-PINV 0.1175030974154046d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9999999999999998d0)
   (MULTIPLE-VALUE-LIST
    (RAYLEIGH-QINV 0.8824969025845955d0 2.0d0))))

