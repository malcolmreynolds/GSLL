;********************************************************
; file:        pareto.lisp                          
; description: Pareto distribution                  
; date:        Sat Oct  8 2006 - 21:23
; author:      Liam M. Healy                             
; modified:    Sun Oct  8 2006 - 21:45
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl pareto (generator a b)
  "gsl_ran_pareto"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation
  "A random variate from the Pareto distribution of order @var{a}.
   The distribution function is
   p(x) dx = (a/b) / (x/b)^{a+1} dx
   @math{x >= b}.")

(defun-gsl pareto-pdf (x a b)
  "gsl_ran_pareto_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a Pareto distribution with exponent @var{a} and scale @var{b}, using
   the formula given in #'pareto.")

(defun-gsl pareto-P (x a b)
  "gsl_cdf_pareto_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for the Pareto distribution with exponent @var{a} and scale @var{b}.")

(defun-gsl pareto-Q (x a b)
  "gsl_cdf_pareto_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for the Pareto distribution with exponent @var{a} and scale @var{b}.")

(defun-gsl pareto-Pinv (P a b)
  "gsl_cdf_pareto_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for the Pareto distribution with exponent @var{a} and scale @var{b}.")

(defun-gsl pareto-Qinv (Q a b)
  "gsl_cdf_pareto_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
   @math{Q(x)} for the Pareto distribution with exponent @var{a} and scale @var{b}.")

;;; Examples and unit test
(lisp-unit:define-test pareto
  (lisp-unit:assert-equal
   '("0.200051663561d+01" "0.122767265962d+02" "0.707669496594d+01"
     "0.211148407447d+01" "0.863347081110d+01" "0.412393569645d+01"
     "0.208882311615d+01" "0.268706924980d+01" "0.370340428797d+01"
     "0.270287443943d+01" "0.263177356639d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (pareto *rng-mt19937* 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.511603440571d+00"
   (pareto-pdf 1.5d0 1.3d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.516884983518d+00"
   (pareto-P 3.5d0 1.3d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.483115016482d+00"
   (pareto-Q 3.5d0 1.3d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.350000000000d+01"
   (pareto-Pinv 0.5168849835182453d0 1.3d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.350000000000d+01"
   (pareto-Qinv 0.4831150164817547d0 1.3d0 2.0d0)))
