;********************************************************
; file:        cauchy.lisp                          
; description: Cauchy distribution                  
; date:        Sat Sep 30 2006
; author:      Liam M. Healy                             
; modified:    Sat Sep 30 2006 - 19:15
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl cauchy (generator a)
  "gsl_ran_cauchy"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation
  "A random variate from the Cauchy distribution with
   scale parameter @var{a}.  The probability distribution for Cauchy
   random variates is,
   p(x) dx = {1 \over a\pi (1 + (x/a)^2) } dx
   for @math{x} in the range @math{-\infty} to @math{+\infty}.  The Cauchy
   distribution is also known as the Lorentz distribution.")

(defun-gsl cauchy-pdf (x a)
  "gsl_ran_cauchy_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a Cauchy distribution with scale parameter @var{a}, using the formula
   given for #'cauchy.")

(defun-gsl cauchy-P (x a)
  "gsl_cdf_cauchy_P" ((x :double) (a :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for the Cauchy distribution with scale parameter @var{a}.")

(defun-gsl cauchy-Q (x a)
  "gsl_cdf_cauchy_Q" ((x :double) (a :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for the Cauchy distribution with scale parameter @var{a}.")

(defun-gsl cauchy-Pinv (P a)
  "gsl_cdf_cauchy_Pinv" ((P :double) (a :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for the Cauchy distribution with scale parameter @var{a}.")

(defun-gsl cauchy-Qinv (Q a)
  "gsl_cdf_cauchy_Qinv" ((Q :double) (a :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
   @math{Q(x)} for the Cauchy distribution with scale parameter @var{a}.")

;;; Examples and unit test
(lisp-unit:define-test cauchy
  (lisp-unit:assert-equal
   '("-0.811319915595d-02" "0.561719641059d+01" "0.122923698289d+02"
     "-0.167410883578d+01" "0.890910448626d+01" "0.211676586154d+03"
     "-0.134390491844d+01" "-0.103643632829d+02" "-0.790709314248d+02"
     "-0.106520710880d+02" "-0.939394824349d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (cauchy *rng-mt19937* 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.318309886184d-01"
   (cauchy-pdf 0.0d0 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.647583617650d+00"
   (cauchy-P 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.352416382350d+00"
   (cauchy-Q 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "1.000000000000d+00"
   (cauchy-Pinv 0.6475836176504333d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+01"
   (cauchy-Qinv 0.35241638234956674d0 2.0d0)))
