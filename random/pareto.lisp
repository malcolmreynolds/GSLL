;; Pareto distribution
;; Liam Healy, Sat Oct  8 2006 - 21:23
;; Time-stamp: <2008-12-26 19:47:19EST pareto.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h
;;; /usr/include/gsl/gsl_cdf.h

(defmfun pareto (generator a b)
  "gsl_ran_pareto"
  (((mpointer generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Pareto distribution of order a.
   The distribution function is
   p(x) dx = (a/b) / (x/b)^{a+1} dx
   x >= b.")

(defmfun pareto-pdf (x a b)
  "gsl_ran_pareto_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Pareto distribution with exponent a and scale b, using
   the formula given in #'pareto.")

(defmfun pareto-P (x a b)
  "gsl_cdf_pareto_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the Pareto distribution with exponent a and scale b.")

(defmfun pareto-Q (x a b)
  "gsl_cdf_pareto_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the Pareto distribution with exponent a and scale b.")

(defmfun pareto-Pinv (P a b)
  "gsl_cdf_pareto_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the Pareto distribution with exponent a and scale b.")

(defmfun pareto-Qinv (Q a b)
  "gsl_cdf_pareto_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the Pareto distribution with exponent a and scale b.")

;;; Examples and unit test
(save-test pareto
  (let ((rng (make-random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (pareto rng 1.0d0 2.0d0)))
  (pareto-pdf 1.5d0 1.3d0 1.0d0)
  (pareto-P 3.5d0 1.3d0 2.0d0)
  (pareto-Q 3.5d0 1.3d0 2.0d0)
  (pareto-Pinv 0.5168849835182453d0 1.3d0 2.0d0)
  (pareto-Qinv 0.4831150164817547d0 1.3d0 2.0d0))
