;; Flat distribution
;; Liam Healy, Oct  7 2006
;; Time-stamp: <2008-02-03 11:18:30EST flat.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl flat (generator a b)
  "gsl_ran_flat"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the flat (uniform)
   distribution from a to b.  The distribution is
   p(x) dx = {1 \over (b-a)} dx
   if a <= x < b, and 0 otherwise.")

(defun-gsl flat-pdf (x a b)
  "gsl_ran_flat_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a uniform distribution from a to b, using the formula
   given for #'flat.")

(defun-gsl flat-P (x a b)
  "gsl_cdf_flat_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   P(x) for a uniform distribution from a to b.")

(defun-gsl flat-Q (x a b)
  "gsl_cdf_flat_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(x) for a uniform distribution from a to b.")

(defun-gsl flat-Pinv (P a b)
  "gsl_cdf_flat_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   P(x) for a uniform distribution from a to b.")

(defun-gsl flat-Qinv (Q a b)
  "gsl_cdf_flat_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for a uniform distribution from a to b.")

;;; Examples and unit test
(lisp-unit:define-test flat
  (lisp-unit:assert-equal
   '("0.199974174891d+01" "0.116290987539d+01" "0.128261780529d+01"
     "0.194720108202d+01" "0.123165654275d+01" "0.148497361434d+01"
     "0.195747695654d+01" "0.174430534313d+01" "0.154004365834d+01"
     "0.173995298147d+01" "0.175994379818d+01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (flat rng 1.0d0 2.0d0)))))
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
