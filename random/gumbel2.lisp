;; The Gumbel type 2 random number distribution
;; Liam Healy, Sun Oct 29 2006
;; Time-stamp: <2008-10-25 13:38:04EDT gumbel2.lisp>
;; $Id$

(in-package :gsl)

(defmfun gumbel2 (generator a b)
  "gsl_ran_gumbel2"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Type-2 Gumbel
   distribution, p(x) dx = a b x^{-a-1} \exp(-b x^{-a}) dx
   for 0 < x < \infty.")

(defmfun gumbel2-pdf (x a b)
  "gsl_ran_gumbel2_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Type-2 Gumbel distribution with parameters a and b,
   using the formula given in #'gumbel2.")

(defmfun gumbel2-P (x a b)
  "gsl_cdf_gumbel2_P" ((x :double) (a :double) (b :double))
  :c-return :double			; FDL
  :documentation "The cumulative distribution functions
  P(x) for the Type-2 Gumbel distribution with
  parameters a and b.")

(defmfun gumbel2-Q (x a b)
  "gsl_cdf_gumbel2_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the Type-2 Gumbel distribution with
  parameters a and b.")

(defmfun gumbel2-Pinv (P a b)
  "gsl_cdf_gumbel2_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the Type-2 Gumbel distribution with
  parameters a and b.")

(defmfun gumbel2-Qinv (Q a b)
  "gsl_cdf_gumbel2_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  Q(x) for the Type-2 Gumbel distribution with
  parameters a and b.")

;;; Examples and unit test
(save-test gumbel2
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (gumbel2 rng 1.0d0 2.0d0)))
  (gumbel2-pdf 5.0d0 1.0d0 2.0d0)
  (gumbel2-P 10.0d0 1.0d0 2.0d0)
  (gumbel2-Q 10.0d0 1.0d0 2.0d0)
  (gumbel2-Pinv 0.8187307530779818d0 1.0d0 2.0d0)
  (gumbel2-Qinv 0.18126924692201815d0 1.0d0 2.0d0))
