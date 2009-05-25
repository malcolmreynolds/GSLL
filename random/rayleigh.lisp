;; Rayleigh distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2009-05-24 20:05:46EDT rayleigh.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h

(defmfun sample
    ((generator random-number-generator) (type (eql 'rayleigh))
     &key sigma)
  "gsl_ran_rayleigh"
  (((mpointer generator) :pointer) (sigma :double))
  :definition :method
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
(save-test rayleigh
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect
	    (sample rng 'rayleigh :sigma 10.0d0)))
  (rayleigh-pdf 0.5d0 1.0d0)
  (rayleigh-P 1.0d0 2.0d0)
  (rayleigh-Q 1.0d0 2.0d0)
  (rayleigh-Pinv 0.1175030974154046d0 2.0d0)
  (rayleigh-Qinv 0.8824969025845955d0 2.0d0))
