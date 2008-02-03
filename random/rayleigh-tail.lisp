;; Rayleigh tail distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-02 23:00:39EST rayleigh-tail.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl rayleigh-tail (generator a sigma)
  "gsl_ran_rayleigh_tail"
  (((generator generator) :pointer) (a :double) (sigma :double))
  :c-return :double
  :documentation
  "A random variate from the tail of the Rayleigh
  distribution with scale parameter @var{sigma} and a lower limit of
  @var{a}.  The distribution is
  p(x) dx = {x \over \sigma^2} \exp ((a^2 - x^2) /(2 \sigma^2)) dx
  for @math{x > a}.")

(defun-gsl rayleigh-tail-pdf (x a sigma)
  "gsl_ran_rayleigh_tail_pdf" ((x :double) (a :double) (sigma :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a Rayleigh tail distribution with scale parameter @var{sigma} and
   lower limit @var{a}, using the formula given in #'rayleigh-tail.")

;;; Examples and unit test
(lisp-unit:define-test rayleigh-tail
  (lisp-unit:assert-equal
   '("0.102550323704d+01" "0.190764679267d+02" "0.159289661023d+02"
     "0.344220488991d+01" "0.171318383334d+02" "0.120719575294d+02"
     "0.311299291669d+01" "0.774988930120d+01" "0.111454501381d+02"
     "0.782519818732d+01" "0.747677468155d+01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect (rayleigh-tail rng 1.0d0 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.102243176249d+00"
   (rayleigh-tail-pdf 0.25d0 -2.0d0 2.0d0)))
