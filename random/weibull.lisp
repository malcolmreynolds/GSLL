;********************************************************
; file:        weibull.lisp                          
; description: Weibull distribution                  
; date:        Sun Oct 22 2006
; author:      Liam M. Healy                             
; modified:    Sat Nov 11 2006 - 20:59
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl weibull (generator a b)
  "gsl_ran_weibull"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation
  "A random variate from the Weibull distribution.  The distribution function is
   p(x) dx = {b \over a^b} x^{b-1}  \exp(-(x/a)^b) dx
   for @math{x >= 0}.")

(defun-gsl weibull-pdf (x a b)
  "gsl_ran_weibull_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a Weibull distribution with scale @var{a} and exponent @var{b},
   using the formula given in #'weibull.")

(defun-gsl weibull-P (x a b)
  "gsl_cdf_weibull_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for the Weibull distribution with scale @var{a} and exponent @var{b}.")

(defun-gsl weibull-Q (x a b)
  "gsl_cdf_weibull_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for the Weibull distribution with scale @var{a} and exponent @var{b}.")

(defun-gsl weibull-Pinv (P a b)
  "gsl_cdf_weibull_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for the Weibull distribution scale @var{a} and exponent @var{b}.")

(defun-gsl weibull-Qinv (Q a b)
  "gsl_cdf_weibull_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
   @math{Q(x)} for the Weibull distribution exponent @var{a} and scale @var{b}.")

;;; Examples and unit test
(lisp-unit:define-test weibull
  (lisp-unit:assert-equal
   '("0.160712303787d-01" "0.134705535996d+01" "0.112412624088d+01"
     "0.232903139783d+00" "0.120933842386d+01" "0.850682545344d+00"
     "0.208455329740d+00" "0.543418734407d+00" "0.784923750377d+00"
     "0.548788332013d+00" "0.523937780842d+00")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (weibull *rng-mt19937* 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.242631749722d+00"
   (weibull-pdf 1.5d0 1.3d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.999288774280d+00"
   (weibull-P 3.5d0 1.3d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.711225720092d-03"
   (weibull-Q 3.5d0 1.3d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.350000000000d+01"
   (weibull-Pinv 0.9992887742799077d0 1.3d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.350000000000d+01"
   (weibull-Qinv 7.112257200923508d-4 1.3d0 2.0d0)))
