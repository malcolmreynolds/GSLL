;; Logistic distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2008-12-26 11:45:08EST logistic.lisp>
;; $Id$

(in-package :gsl)

(defmfun logistic (generator a)
  "gsl_ran_logistic"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the logistic distribution.  The distribution function is
   p(x) dx = { \exp(-x/a) \over a (1 + \exp(-x/a))^2 } dx
   for -\infty < x < +\infty.")

(defmfun logistic-pdf (x a)
  "gsl_ran_logistic_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a logistic distribution with scale parameter a, using the
   formula given in #'logistic.")

(defmfun logistic-P (x a)
  "gsl_cdf_logistic_P" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the logistic distribution with scale parameter a.")

(defmfun logistic-Q (x a)
  "gsl_cdf_logistic_Q" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the logistic distribution with scale parameter a.")

(defmfun logistic-Pinv (P a)
  "gsl_cdf_logistic_Pinv" ((P :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the logistic distribution with scale parameter a.")

(defmfun logistic-Qinv (Q a)
  "gsl_cdf_logistic_Qinv" ((Q :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the logistic distribution with scale parameter a.")

;;; Examples and unit test
(save-test logistic
  (let ((rng (make-random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (logistic rng 10.0d0)))
  (logistic-pdf 0.5d0 1.0d0)
  (logistic-P 0.5d0 1.0d0)
  (logistic-Q 0.5d0 1.0d0)
  (logistic-Pinv 0.6224593312018546d0 1.0d0)
  (logistic-Qinv 0.37754066879814546d0 1.0d0))
