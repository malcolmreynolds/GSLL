;; Exponential distribution
;; Liam Healy, Sun Sep 17 2006
;; Time-stamp: <2008-10-25 13:42:41EDT laplace.lisp>
;; $Id$

(in-package :gsl)

(defmfun laplace (generator a)
  "gsl_ran_laplace"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Laplace distribution with width a.
   The distribution is
   p(x) dx = {1 \over 2 a}  \exp(-|x/a|) dx
   for -\infty < x < \infty.")

(defmfun laplace-pdf (x a)
  "gsl_ran_laplace_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Laplace distribution with width a, using the formula
   given for #'laplace.")

(defmfun laplace-P (x a)
  "gsl_cdf_laplace_P" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
   P(x) for the laplace distribution with width a.")

(defmfun laplace-Q (x a)
  "gsl_cdf_laplace_Q" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
   Q(x) for the laplace distribution with width a.")

(defmfun laplace-Pinv (P a)
  "gsl_cdf_laplace_Pinv" ((P :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function
   P(x) for the laplace distribution with width a.")

(defmfun laplace-Qinv (Q a)
  "gsl_cdf_laplace_Qinv" ((Q :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function
   Q(x) for the laplace distribution with width a.")

;;; Examples and unit test
(save-test laplace
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (laplace rng 10.0d0)))
  (laplace-pdf 0.0d0 10.0d0)
  (laplace-p 1.0d0 2.0d0)
  (laplace-q 1.0d0 2.0d0)
  (laplace-pinv 0.6967346701436833d0 2.0d0)
  (laplace-qinv 0.3032653298563167d0 2.0d0))
