;; Exponential distribution
;; Liam Healy, Sat Sep  2 2006 - 19:04
;; Time-stamp: <2008-10-25 13:21:30EDT exponential.lisp>
;; $Id$

(in-package :gsl)

(defmfun exponential (generator mu)
  "gsl_ran_exponential"
  (((generator generator) :pointer) (mu :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the exponential distribution
   with mean mu. The distribution is
   p(x) dx = {1 \over \mu} \exp(-x/\mu) dx
   x >= 0.")

(defmfun exponential-pdf (x mu)
  "gsl_ran_exponential_pdf" ((x :double) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
  for an exponential distribution with mean mu, using the formula
  given for exponential.")

(defmfun exponential-P (x mu)
  "gsl_cdf_exponential_P" ((x :double) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
   P(x) for the exponential distribution with mean mu.")

(defmfun exponential-Q (x mu)
  "gsl_cdf_exponential_Q" ((x :double) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
   Q(x) for the exponential distribution with mean mu.")

(defmfun exponential-Pinv (P mu)
  "gsl_cdf_exponential_Pinv" ((P :double) (mu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function
   P(x) for the exponential distribution with mean mu.")

(defmfun exponential-Qinv (Q mu)
  "gsl_cdf_exponential_Qinv" ((Q :double) (mu :double))
  :c-return :double
  :documentation 			; FDL
  "The inverse cumulative distribution function
   Q(x) for the exponential distribution with mean mu.")

;;; Examples and unit test
(save-test exponential
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (exponential rng 10.0d0)))
  (exponential-pdf 0.0d0 10.0d0)
  (exponential-p 1.0d0 2.0d0)
  (exponential-q 1.0d0 2.0d0)
  (exponential-pinv 0.3934693402873666d0 2.0d0)
  (exponential-qinv 0.6065306597126334d0 2.0d0))

