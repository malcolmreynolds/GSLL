;********************************************************
; file:        gumbel1.lisp
; description: Beta distribution                  
; date:        Sun Oct 29 2006
; author:      Liam M. Healy                             
; modified:    Sun Oct 29 2006 - 21:07
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl gumbel1 (generator a b)
  "gsl_ran_gumbel1"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation
  "A random variate from the Type-1 Gumbel
   distribution,
   p(x) dx = a b \exp(-(b \exp(-ax) + ax)) dx
   for @math{-\infty < x < \infty}.")

(defun-gsl gumbel1-pdf (x a b)
  "gsl_ran_gumbel1_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
  for a Type-1 Gumbel distribution with parameters @var{a} and @var{b},
  using the formula given for #'gumbel1.")

(defun-gsl gumbel1-P (x a b)
  "gsl_cdf_gumbel1_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for the Type-1 Gumbel distribution with
  parameters @var{a} and @var{b}.")

(defun-gsl gumbel1-Q (x a b)
  "gsl_cdf_gumbel1_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for the Type-1 Gumbel distribution with
  parameters @var{a} and @var{b}.")

(defun-gsl gumbel1-Pinv (P a b)
  "gsl_cdf_gumbel1_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for the Type-1 Gumbel distribution with
  parameters @var{a} and @var{b}.")

(defun-gsl gumbel1-Qinv (Q a b)
  "gsl_cdf_gumbel1_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{Q(x)} for the Type-1 Gumbel distribution with
  parameters @var{a} and @var{b}.")

;;; Examples and unit test
(lisp-unit:define-test gumbel1
  (lisp-unit:assert-equal   
   '("0.895459625749d+01" "0.973051899751d-01" "0.459135062331d+00"
     "0.360741242243d+01" "0.313000274682d+00" "0.101657969497d+01"
     "0.382920819366d+01" "0.191289739318d+01" "0.117748457895d+01"
     "0.189323210797d+01" "0.198591186168d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	 collect
	 (gumbel1 *rng-mt19937* 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.296257089650d+00"
   (gumbel1-pdf 0.1 1.0 2.0))
  (lisp-unit:assert-first-fp-equal
   "0.163707359877d+00"
   (gumbel1-P 0.1 1.0 2.0))
  (lisp-unit:assert-first-fp-equal
   "0.836292640123d+00"
   (gumbel1-Q 0.1 1.0 2.0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+00"
   (gumbel1-Pinv 0.1637073598773166 1.0 2.0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+00"
   (gumbel1-Qinv 0.8362926401226833 1.0 2.0)))
