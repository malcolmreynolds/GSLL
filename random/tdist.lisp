;; Tdist distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2008-10-25 18:18:44EDT tdist.lisp>
;; $Id$

(in-package :gsl)

(defmfun tdist (generator nu)
  "gsl_ran_tdist"
  (((generator generator) :pointer) (nu :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Student t-distribution.  The
   distribution function is,
   p(x) dx = {\Gamma((\nu + 1)/2) \over \sqrt{\pi \nu} \Gamma(\nu/2)}
   (1 + x^2/\nu)^{-(\nu + 1)/2} dx
   for -\infty < x < +\infty.")

(defmfun tdist-pdf (x nu)
  "gsl_ran_tdist_pdf" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a t-distribution with nu degrees of freedom, using the formula
   given in #'tdist.")

(defmfun tdist-P (x nu)
  "gsl_cdf_tdist_P" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the tdist distribution with nu degrees of freedom.")

(defmfun tdist-Q (x nu)
  "gsl_cdf_tdist_Q" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(x) for the tdist distribution with nu degrees of freedom.")

(defmfun tdist-Pinv (P nu)
  "gsl_cdf_tdist_Pinv" ((P :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   P(x) for the tdist distribution with nu degrees of freedom.")

(defmfun tdist-Qinv (Q nu)
  "gsl_cdf_tdist_Qinv" ((Q :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the tdist distribution with nu degrees of freedom.")

;;; Examples and unit test
(save-test tdist
  (letm ((rng (random-number-generator *mt19937* 0)))
      (rng-set rng 0)
      (loop for i from 0 to 10
	    collect
	    (tdist rng 10.0d0)))
  (tdist-pdf 0.5d0 1.0d0)
  (tdist-P 0.5d0 1.0d0)
  (tdist-Q 0.5d0 1.0d0)
  (tdist-Pinv 0.6475836176504334d0 1.0d0)
  (tdist-Qinv 0.3524163823495667d0 1.0d0))
