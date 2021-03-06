;; Poisson distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2009-05-24 21:58:22EDT poisson.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h
;;; /usr/include/gsl/gsl_cdf.h

(defmfun sample
    ((generator random-number-generator) (type (eql 'poisson)) &key mu)
  "gsl_ran_poisson"
  (((mpointer generator) :pointer) (mu :double))
  :definition :method
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
(save-test poisson
  (let ((rng (make-random-number-generator +mt19937+ 0)))
     (loop for i from 0 to 10
	   collect
	   (sample rng 'poisson :mu 10.0d0)))
  (poisson-pdf 8 10.0d0)
  (poisson-P 8 10.0d0)
  (poisson-Q 8 10.0d0))
