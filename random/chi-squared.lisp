;********************************************************
; file:        chi-squared.lisp                          
; description: Chi-squared distribution                  
; date:        Sat Oct  7 2006 - 16:13
; author:      Liam M. Healy                             
; modified:    Sun Oct  8 2006 - 16:41
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl chi-squared (generator nu)
  "gsl_ran_chisq"
  (((generator generator) :pointer) (nu :double))
  :c-return :double
  :documentation
  "A random variate from the chi-squared distribution
  with @var{nu} degrees of freedom. The distribution function is
  p(x) dx = {1 \over 2 \Gamma(\nu/2) } (x/2)^{\nu/2 - 1} \exp(-x/2) dx
  @math{x >= 0}. ")

(defun-gsl chi-squared-pdf (x nu)
  "gsl_ran_chisq_pdf" ((x :double) (nu :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a chi-squared distribution with @var{nu} degrees of freedom, using
   the formula given in #'chi-squared.")

(defun-gsl chi-squared-P (x nu)
  "gsl_cdf_chisq_P" ((x :double) (nu :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for the chi-squared distribution with @var{nu} degrees of freedom.")

(defun-gsl chi-squared-Q (x nu)
  "gsl_cdf_chisq_Q" ((x :double) (nu :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for the chi-squared distribution with @var{nu} degrees of freedom.")

(defun-gsl chi-squared-Pinv (P nu)
  "gsl_cdf_chisq_Pinv" ((P :double) (nu :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for the chi-squared distribution with @var{nu} degrees of freedom.")

(defun-gsl chi-squared-Qinv (Q nu)
  "gsl_cdf_chisq_Qinv" ((Q :double) (nu :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
   @math{Q(x)} for the chi-squared distribution with @var{nu} degrees of freedom.")

;;; Examples and unit test
(lisp-unit:define-test chi-squared
  (lisp-unit:assert-equal
   '("0.919043905723d+01" "0.395938453123d+01" "0.543493338508d+01"
     "0.130231074199d+02" "0.136706908417d+02" "0.698235971535d+01"
     "0.137566397216d+02" "0.113594580454d+02" "0.160682825906d+02"
     "0.129995142525d+02" "0.111473474244d+02")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (chi-squared *rng-mt19937* 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.439391289468d+00"
   (chi-squared-pdf 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.520499877813d+00"
   (chi-squared-P 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.479500122187d+00"
   (chi-squared-Q 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d+00"
   (chi-squared-Pinv 0.5204998778130463d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d+00"
   (chi-squared-Qinv 0.4795001221869537d0 1.0d0)))
