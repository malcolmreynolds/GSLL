;; Poisson distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-02-17 13:38:14EST poisson.lisp>
;; $Id: $

(in-package :gsl)

(defmfun poisson (generator mu)
  "gsl_ran_poisson"
  (((generator generator) :pointer) (mu :double))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the Poisson distribution with mean mu.
   The probability distribution for Poisson variates is
   p(k) = {\mu^k \over k!} \exp(-\mu)
   k >= 0.")

(defmfun poisson-pdf (k mu)
  "gsl_ran_poisson_pdf" ((k :uint) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a Poisson distribution with mean mu using the formula
   given in #'poisson.")

(defmfun poisson-P (k mu)
  "gsl_cdf_poisson_P" ((k :uint) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the Poisson distribution with parameter mu.")

(defmfun poisson-Q (k mu)
  "gsl_cdf_poisson_Q" ((k :uint) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(k) for the Poisson distribution with parameter mu.")

;;; Examples and unit test
#|
(make-tests poisson
  (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (poisson rng 10.0d0)))
  (poisson-pdf 8 10.0d0)
  (poisson-P 8 10.0d0)
  (poisson-Q 8 10.0d0))
|#

(LISP-UNIT:DEFINE-TEST POISSON
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 15 6 9 9 5 8 11 9 11 5 10))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (POISSON RNG 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.11259903214902009d0)
   (MULTIPLE-VALUE-LIST (POISSON-PDF 8 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.3328196787507177d0)
   (MULTIPLE-VALUE-LIST (POISSON-P 8 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6671803212492823d0)
   (MULTIPLE-VALUE-LIST (POISSON-Q 8 10.0d0))))

