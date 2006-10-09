;********************************************************
; file:        logistic.lisp                          
; description: logistic distribution                  
; date:        Sat Oct  7 2006 - 16:13
; author:      Liam M. Healy                             
; modified:    Sun Oct  8 2006 - 17:32
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl logistic (generator a)
  "gsl_ran_logistic"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation
  "A random variate from the logistic distribution.  The distribution function is
   p(x) dx = { \exp(-x/a) \over a (1 + \exp(-x/a))^2 } dx
   for @math{-\infty < x < +\infty}.")

(defun-gsl logistic-pdf (x a)
  "gsl_ran_logistic_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a logistic distribution with scale parameter @var{a}, using the
   formula given in #'logistic.")

(defun-gsl logistic-P (x a)
  "gsl_cdf_logistic_P" ((x :double) (a :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for the logistic distribution with scale parameter @var{a}.")

(defun-gsl logistic-Q (x a)
  "gsl_cdf_logistic_Q" ((x :double) (a :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for the logistic distribution with scale parameter @var{a}.")

(defun-gsl logistic-Pinv (P a)
  "gsl_cdf_logistic_Pinv" ((P :double) (a :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for the logistic distribution with scale parameter @var{a}.")

(defun-gsl logistic-Qinv (Q a)
  "gsl_cdf_logistic_Qinv" ((Q :double) (a :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
   @math{Q(x)} for the logistic distribution with scale parameter @var{a}.")

;;; Examples and unit test
(lisp-unit:define-test logistic
  (lisp-unit:assert-equal
   '("0.826131993192d+02" "-0.163673460427d+02" "-0.931513272044d+01"
     "0.288702070871d+02" "-0.119898098758d+02" "-0.601236476200d+00"
     "0.311425552636d+02" "0.106846737210d+02" "0.160518409540d+01"
     "0.104572419047d+02" "0.115237141063d+02")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (logistic *rng-mt19937* 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.235003712202d+00"
   (logistic-pdf 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.622459331202d+00"
   (logistic-P 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.377540668798d+00"
   (logistic-Q 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d+00"
   (logistic-Pinv 0.6224593312018546d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d+00"
   (logistic-Qinv 0.37754066879814546d0 1.0d0)))
