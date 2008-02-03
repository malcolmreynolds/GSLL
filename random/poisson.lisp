;; Poisson distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-02-03 09:50:16EST poisson.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl poisson (generator mu)
  "gsl_ran_poisson"
  (((generator generator) :pointer) (mu :double))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the Poisson distribution with mean mu.
   The probability distribution for Poisson variates is
   p(k) = {\mu^k \over k!} \exp(-\mu)
   k >= 0.")

(defun-gsl poisson-pdf (k mu)
  "gsl_ran_poisson_pdf" ((k :uint) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a Poisson distribution with mean mu using the formula
   given in #'poisson.")

(defun-gsl poisson-P (k mu)
  "gsl_cdf_poisson_P" ((k :uint) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the Poisson distribution with parameter mu.")

(defun-gsl poisson-Q (k mu)
  "gsl_cdf_poisson_Q" ((k :uint) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(k) for the Poisson distribution with parameter mu.")

;;; Examples and unit test
(lisp-unit:define-test poisson
  (lisp-unit:assert-equal
   '(15 6 9 9 5 8 11 9 11 5 10)
   (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (poisson rng 10.0d0))))
  (lisp-unit:assert-first-fp-equal
   "0.112599032149d+00"
   (poisson-pdf 8 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.332819678751d+00"
   (poisson-P 8 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.667180321249d+00"
   (poisson-Q 8 10.0d0)))
