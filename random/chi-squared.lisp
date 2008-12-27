;; Chi-squared distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2008-12-26 19:47:17EST chi-squared.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h
;;; /usr/include/gsl/gsl_cdf.h

(defmfun chi-squared (generator nu)
  "gsl_ran_chisq"
  (((mpointer generator) :pointer) (nu :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the chi-squared distribution
  with nu degrees of freedom. The distribution function is
  p(x) dx = {1 \over 2 \Gamma(\nu/2) } (x/2)^{\nu/2 - 1} \exp(-x/2) dx
  x >= 0. ")

(defmfun chi-squared-pdf (x nu)
  "gsl_ran_chisq_pdf" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a chi-squared distribution with nu degrees of freedom, using
   the formula given in #'chi-squared.")

(defmfun chi-squared-P (x nu)
  "gsl_cdf_chisq_P" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the chi-squared distribution with nu degrees of freedom.")

(defmfun chi-squared-Q (x nu)
  "gsl_cdf_chisq_Q" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the chi-squared distribution with nu degrees of freedom.")

(defmfun chi-squared-Pinv (P nu)
  "gsl_cdf_chisq_Pinv" ((P :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the chi-squared distribution with nu degrees of freedom.")

(defmfun chi-squared-Qinv (Q nu)
  "gsl_cdf_chisq_Qinv" ((Q :double) (nu :double))
  :c-return :double
  :documentation 			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the chi-squared distribution with nu degrees of freedom.")

;;; Examples and unit test
(save-test chi-squared
  (let ((rng (make-random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (chi-squared rng 10.0d0)))
  (chi-squared-pdf 0.5d0 1.0d0)
  (chi-squared-P 0.5d0 1.0d0)
  (chi-squared-Q 0.5d0 1.0d0)
  (chi-squared-Pinv 0.5204998778130463d0 1.0d0)
  (chi-squared-Qinv 0.4795001221869537d0 1.0d0))



