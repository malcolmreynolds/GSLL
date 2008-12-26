;; Exponential power distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-12-26 11:41:54EST exponential-power.lisp>
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
(save-test exponential-power
  (let ((rng (make-random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (exponential-power rng 1.0d0 2.0d0)))
  (exponential-power-pdf 0.0d0 1.0d0 2.0d0)
  (exponential-power-P 1.0d0 1.0d0 2.0d0)
  (exponential-power-Q 1.0d0 1.0d0 2.0d0))

