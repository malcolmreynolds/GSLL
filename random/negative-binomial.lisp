;********************************************************
; file:        negative-binomial.lisp                          
; description: Negative binomial and Pascal distributions
; date:        Sat Nov 25 2006 - 16:00
; author:      Liam M. Healy                             
; modified:    Sat Nov 25 2006 - 21:42
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Negative binomial
;;;;****************************************************************************

(defun-gsl negative-binomial (generator p n)
  "gsl_ran_negative_binomial"
  (((generator generator) :pointer) (p :double) (n :double))
  :c-return :uint
  :documentation
  "A random integer from the negative binomial
   distribution, the number of failures occurring before @var{n} successes
   in independent trials with probability @var{p} of success.  The
   probability distribution for negative binomial variates is,
   p(k) = {\Gamma(n + k) \over \Gamma(k+1) \Gamma(n) } p^n (1-p)^k
   Note that @math{n} is not required to be an integer.")

(defun-gsl negative-binomial-pdf (k p n)
  "gsl_ran_negative_binomial_pdf" ((k :uint) (p :double) (n :double))
  :c-return :double
  :documentation
  "The probability @math{p(k)} of obtaining @var{k}
   from a negative binomial distribution with parameters @var{p} and
   @var{n}, using the formula given in #'negative-binomial.")

(defun-gsl negative-binomial-P (k p n)
  "gsl_cdf_negative_binomial_P" ((k :uint) (p :double) (n :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(k)} for the negative binomial distribution
  with parameters @var{p} and @var{n}.")

(defun-gsl negative-binomial-Q (k p n)
  "gsl_cdf_negative_binomial_Q" ((k :uint) (p :double) (n :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(k)} for the negative binomial distribution
   with parameters @var{p} and @var{n}.")

;;;;****************************************************************************
;;;; Pascal
;;;;****************************************************************************

(defun-gsl pascal (generator p n)
  "gsl_ran_pascal"
  (((generator generator) :pointer) (p :double) (n :uint))
  :c-return :uint
  :documentation
  "A random integer from the Pascal distribution.  The
   Pascal distribution is simply a negative binomial distribution with an
   integer value of @math{n}.
   p(k) = {(n + k - 1)! \over k! (n - 1)! } p^n (1-p)^k
   @math{k >= 0}.")

(defun-gsl pascal-pdf (k p n)
  "gsl_ran_pascal_pdf" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation
  "The probability @math{p(k)} of obtaining @var{k}
   from a Pascal distribution with parameters @var{p} and
   @var{n}, using the formula given in #'pascal.")

(defun-gsl pascal-P (k p n)
  "gsl_cdf_pascal_P" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(k)} for the Pascal distribution
  with parameters @var{p} and @var{n}.")

(defun-gsl pascal-Q (k p n)
  "gsl_cdf_pascal_Q" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(k)} for the Pascal distribution
   with parameters @var{p} and @var{n}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test negative-binomial
  (lisp-unit:assert-equal
   '(10 7 12 23 20 24 18 12 4 22 15)
   (progn
     (rng-set *rng-mt19937* 0)
     (loop for i from 0 to 10
	collect
	(negative-binomial *rng-mt19937* 0.4d0 12.0d0))))
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
   (progn
     (rng-set *rng-mt19937* 0)
     (loop for i from 0 to 10
	collect
	(pascal *rng-mt19937* 0.4d0 12))))
  (lisp-unit:assert-first-fp-equal
   "0.569847670899d-02"
   (pascal-pdf 5 0.4d0 12))
  (lisp-unit:assert-first-fp-equal
   "0.105942025555d-01"
   (pascal-P 5 0.4d0 12))
  (lisp-unit:assert-first-fp-equal
   "0.989405797444d+00"
   (pascal-Q 5 0.4d0 12)))
