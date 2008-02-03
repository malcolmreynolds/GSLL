;; Rayleigh distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-03 11:29:48EST rayleigh.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl rayleigh (generator sigma)
  "gsl_ran_rayleigh"
  (((generator generator) :pointer) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Rayleigh distribution with
   scale parameter sigma.  The distribution is
   p(x) dx = {x \over \sigma^2} \exp(- x^2/(2 \sigma^2)) dx
   for x > 0.")

(defun-gsl rayleigh-pdf (x sigma)
  "gsl_ran_rayleigh_pdf" ((x :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Rayleigh distribution with scale parameter sigma, using the
   formula given for #'rayleigh.")

(defun-gsl rayleigh-P (x sigma)
  "gsl_cdf_rayleigh_P" ((x :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
  P(x) for the Rayleigh distribution with scale
  parameter sigma.")

(defun-gsl rayleigh-Q (x sigma)
  "gsl_cdf_rayleigh_Q" ((x :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
  Q(x) for the Rayleigh distribution with scale
  parameter sigma.")

(defun-gsl rayleigh-Pinv (P sigma)
  "gsl_cdf_rayleigh_Pinv" ((P :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function
  P(x)} for the Rayleigh distribution with scale
  parameter sigma.")

(defun-gsl rayleigh-Qinv (Q sigma)
  "gsl_cdf_rayleigh_Qinv" ((Q :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function
  Q(x) for the Rayleigh distribution with scale
  parameter sigma.")

;;; Examples and unit test
(lisp-unit:define-test rayleigh
  (lisp-unit:assert-equal
   '("0.227281519655d+00" "0.190502395932d+02" "0.158975457567d+02"
     "0.329374779000d+01" "0.171026280052d+02" "0.120304679290d+02"
     "0.294800354467d+01" "0.768510144246d+01" "0.111004981321d+02"
     "0.776103902005d+01" "0.740959915506d+01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (rayleigh rng 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.441248451292d+00"
   (rayleigh-pdf 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.117503097415d+00"
   (rayleigh-P 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.882496902585d+00"
   (rayleigh-Q 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+01"
   (rayleigh-Pinv 0.1175030974154046d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "1.000000000000d+00"
   (rayleigh-Qinv 0.8824969025845955d0 2.0d0)))
