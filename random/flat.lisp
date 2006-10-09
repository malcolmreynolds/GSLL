;********************************************************
; file:        flat.lisp                          
; description: Flat distribution                  
; date:        Oct  7 2006
; author:      Liam M. Healy                             
; modified:    Sat Oct  7 2006 - 16:13
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl flat (generator a b)
  "gsl_ran_flat"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation
  "A random variate from the flat (uniform)
   distribution from @var{a} to @var{b}. The distribution is,
   p(x) dx = {1 \over (b-a)} dx
   if @c{$a \le x < b$} @math{a <= x < b} and 0 otherwise.")

(defun-gsl flat-pdf (x a b)
  "gsl_ran_flat_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a uniform distribution from @var{a} to @var{b}, using the formula
   given for #'flat.")

(defun-gsl flat-P (x a b)
  "gsl_cdf_flat_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for a uniform distribution from @var{a} to @var{b}.")

(defun-gsl flat-Q (x a b)
  "gsl_cdf_flat_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for a uniform distribution from @var{a} to @var{b}.")

(defun-gsl flat-Pinv (P a b)
  "gsl_cdf_flat_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for a uniform distribution from @var{a} to @var{b}.")

(defun-gsl flat-Qinv (Q a b)
  "gsl_cdf_flat_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
   @math{Q(x)} for a uniform distribution from @var{a} to @var{b}.")

;;; Examples and unit test
(lisp-unit:define-test flat
  (lisp-unit:assert-equal
   '("0.199974174891d+01" "0.116290987539d+01" "0.128261780529d+01"
     "0.194720108202d+01" "0.123165654275d+01" "0.148497361434d+01"
     "0.195747695654d+01" "0.174430534313d+01" "0.154004365834d+01"
     "0.173995298147d+01" "0.175994379818d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (flat *rng-mt19937* 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+01"
   (flat-pdf 1.2d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.200000000000d+00"
   (flat-P 1.2d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.800000000000d+00"
   (flat-Q 1.2d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000d+01"
   (flat-Pinv 0.19999999999999996d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000d+01"
   (flat-Qinv 0.8d0 1.0d0 2.0d0)))
