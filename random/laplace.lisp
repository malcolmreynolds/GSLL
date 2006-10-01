;********************************************************
; file:        exponential.lisp                          
; description: Exponential distribution                  
; date:        Sun Sep 17 2006
; author:      Liam M. Healy                             
; modified:    Sat Sep 30 2006 - 19:17
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl laplace (generator a)
  "gsl_ran_laplace"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation
  "A random variate from the Laplace distribution with width @var{a}.
   The distribution is
   p(x) dx = {1 \over 2 a}  \exp(-|x/a|) dx
   for @math{-\infty < x < \infty}.")

(defun-gsl laplace-pdf (x a)
  "gsl_ran_laplace_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a Laplace distribution with width @var{a}, using the formula
   given for #'laplace.")

(defun-gsl laplace-P (x a)
  "gsl_cdf_laplace_P" ((x :double) (a :double))
  :c-return :double
  :documentation "The cumulative distribution function
   @math{P(x)} for the laplace distribution with width @var{a}.")

(defun-gsl laplace-Q (x a)
  "gsl_cdf_laplace_Q" ((x :double) (a :double))
  :c-return :double
  :documentation "The cumulative distribution function
   @math{Q(x)} for the laplace distribution with width @var{a}.")

(defun-gsl laplace-Pinv (P a)
  "gsl_cdf_laplace_Pinv" ((P :double) (a :double))
  :c-return :double
  :documentation "The inverse cumulative distribution function
   @math{P(x)} for the laplace distribution with width @var{a}.")

(defun-gsl laplace-Qinv (Q a)
  "gsl_cdf_laplace_Qinv" ((Q :double) (a :double))
  :c-return :double
  :documentation "The inverse cumulative distribution function
   @math{Q(x)} for the laplace distribution with width @var{a}.")

;;; Examples and unit test
(lisp-unit:define-test laplace
  (lisp-unit:assert-equal
   '("0.516635619858d-02" "-0.394257771749d+01" "-0.832951028160d+01"
     "0.111599757046d+01" "-0.622340381488d+01" "-0.350480039842d+02"
     "0.888815832003d+00" "0.716189249197d+01" "0.252463778091d+02"
     "0.734165104806d+01" "0.654142651602d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (laplace *rng-mt19937* 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d-01"
   (laplace-pdf 0.0d0 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.696734670144d+00"
   (laplace-p 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.303265329856d+00"
   (laplace-q 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+01"
   (laplace-pinv 0.6967346701436833d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+01"
   (laplace-qinv 0.3032653298563167d0 2.0d0)))
