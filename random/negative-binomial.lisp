;; Negative binomial and Pascal distributions
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-02-03 11:25:48EST negative-binomial.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Negative binomial
;;;;****************************************************************************

(defun-gsl negative-binomial (generator p n)
  "gsl_ran_negative_binomial"
  (((generator generator) :pointer) (p :double) (n :double))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the negative binomial
   distribution, the number of failures occurring before n successes
   in independent trials with probability p of success.  The
   probability distribution for negative binomial variates is,
   p(k) = {\Gamma(n + k) \over \Gamma(k+1) \Gamma(n) } p^n (1-p)^k
   Note that n is not required to be an integer.")

(defun-gsl negative-binomial-pdf (k p n)
  "gsl_ran_negative_binomial_pdf" ((k :uint) (p :double) (n :double))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a negative binomial distribution with parameters p and
   n, using the formula given in #'negative-binomial.")

(defun-gsl negative-binomial-P (k p n)
  "gsl_cdf_negative_binomial_P" ((k :uint) (p :double) (n :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the negative binomial distribution
  with parameters p and n.")

(defun-gsl negative-binomial-Q (k p n)
  "gsl_cdf_negative_binomial_Q" ((k :uint) (p :double) (n :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(k) for the negative binomial distribution
   with parameters p and n.")

;;;;****************************************************************************
;;;; Pascal
;;;;****************************************************************************

(defun-gsl pascal (generator p n)
  "gsl_ran_pascal"
  (((generator generator) :pointer) (p :double) (n :uint))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the Pascal distribution.  The
   Pascal distribution is simply a negative binomial distribution with an
   integer value of n.
   p(k) = {(n + k - 1)! \over k! (n - 1)! } p^n (1-p)^k
   k >= 0.")

(defun-gsl pascal-pdf (k p n)
  "gsl_ran_pascal_pdf" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a Pascal distribution with parameters p and
   n, using the formula given in #'pascal.")

(defun-gsl pascal-P (k p n)
  "gsl_cdf_pascal_P" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the Pascal distribution
  with parameters p and n.")

(defun-gsl pascal-Q (k p n)
  "gsl_cdf_pascal_Q" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(k) for the Pascal distribution
   with parameters p and n.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test negative-binomial
  (lisp-unit:assert-equal
   '(10 7 12 23 20 24 18 12 4 22 15)
   (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (negative-binomial rng 0.4d0 12.0d0))))
  (lisp-unit:assert-first-fp-equal
   "0.569847670899d-02"
   (negative-binomial-pdf 5 0.4d0 12.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.105942025555d-01"
   (negative-binomial-P 5 0.4d0 12.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.989405797444d+00"
   (negative-binomial-Q 5 0.4d0 12.0d0))
  (lisp-unit:assert-equal
   '(10 7 12 23 20 24 18 12 4 22 15)
   (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (pascal rng 0.4d0 12))))
  (lisp-unit:assert-first-fp-equal
   "0.569847670899d-02"
   (pascal-pdf 5 0.4d0 12))
  (lisp-unit:assert-first-fp-equal
   "0.105942025555d-01"
   (pascal-P 5 0.4d0 12))
  (lisp-unit:assert-first-fp-equal
   "0.989405797444d+00"
   (pascal-Q 5 0.4d0 12)))
