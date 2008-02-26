;; Exponential power distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 12:53:01EST exponential-power.lisp>
;; $Id$

(in-package :gsl)

(defmfun exponential-power (generator a b)
  "gsl_ran_exppow"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the exponential power distribution
   with scale parameter a and exponent b.  The distribution is
   p(x) dx = {1 \over 2 a \Gamma(1+1/b)} \exp(-|x/a|^b) dx
   for x >= 0.  For b = 1 this reduces to the Laplace
   distribution.  For b = 2 it has the same form as a gaussian
   distribution, but with a = \sqrt{2} \sigma.")

(defmfun exponential-power-pdf (x a b)
  "gsl_ran_exppow_pdf" 
  ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for an exponential power distribution with scale parameter a
   and exponent b, using the formula given for #'exponential-power.")

(defmfun exponential-power-P (x a b)
  "gsl_cdf_exppow_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
  P(x), for the exponential power distribution with
  parameters a and b.")

(defmfun exponential-power-Q (x a b)
  "gsl_cdf_exppow_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions Q(x)
  for the exponential power distribution with
  parameters a and b.")

;;; Examples and unit test
#|
(make-tests exponential-power
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (exponential-power rng 1.0d0 2.0d0)))
  (exponential-power-pdf 0.0d0 1.0d0 2.0d0)
  (exponential-power-P 1.0d0 1.0d0 2.0d0)
  (exponential-power-Q 1.0d0 1.0d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST EXPONENTIAL-POWER
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.09469475592777954d0 -0.06229680875327071d0
	  1.183985538537803d0 0.5187626019237904d0
	  0.7053564314063956d0 -0.9033303844569821d0
	  -1.6947336289940842d0 -0.4803236108055401d0
	  -0.027641736349912214d0 0.6318391856046153d0
	  -0.012478875227423025d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (EXPONENTIAL-POWER RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5641895835477557d0)
   (MULTIPLE-VALUE-LIST
    (EXPONENTIAL-POWER-PDF 0.0d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9213503964748571d0)
   (MULTIPLE-VALUE-LIST
    (EXPONENTIAL-POWER-P 1.0d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.07864960352514248d0)
   (MULTIPLE-VALUE-LIST
    (EXPONENTIAL-POWER-Q 1.0d0 1.0d0 2.0d0))))
