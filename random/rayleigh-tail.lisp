;; Rayleigh tail distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-12-26 19:50:49EST rayleigh-tail.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h

(defmfun rayleigh-tail (generator a sigma)
  "gsl_ran_rayleigh_tail"
  (((mpointer generator) :pointer) (a :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the tail of the Rayleigh
  distribution with scale parameter sigma and a lower limit of
  a.  The distribution is
  p(x) dx = {x \over \sigma^2} \exp ((a^2 - x^2) /(2 \sigma^2)) dx
  for x > a.")

(defmfun rayleigh-tail-pdf (x a sigma)
  "gsl_ran_rayleigh_tail_pdf" ((x :double) (a :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Rayleigh tail distribution with scale parameter sigma and
   lower limit a, using the formula given in #'rayleigh-tail.")

;;; Examples and unit test
(save-test rayleigh-tail
  (let ((rng (make-random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect (rayleigh-tail rng 1.0d0 10.0d0)))
  (rayleigh-tail-pdf 0.25d0 -2.0d0 2.0d0))
