;; Rayleigh tail distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 13:00:06EST rayleigh-tail.lisp>
;; $Id$

(in-package :gsl)

(defmfun rayleigh-tail (generator a sigma)
  "gsl_ran_rayleigh_tail"
  (((generator generator) :pointer) (a :double) (sigma :double))
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
#|
(make-tests rayleigh-tail
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect (rayleigh-tail rng 1.0d0 10.0d0)))
  (rayleigh-tail-pdf 0.25d0 -2.0d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST RAYLEIGH-TAIL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 1.0255032370386696d0 19.0764679267351d0
	  15.928966102255199d0 3.4422048899106383d0
	  17.131838333441106d0 12.071957529361999d0
	  3.112992916690818d0 7.749889301203328d0
	  11.145450138119857d0 7.825198187316554d0
	  7.476774681552917d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (RAYLEIGH-TAIL RNG 1.0d0 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.10224317624874313d0)
   (MULTIPLE-VALUE-LIST
    (RAYLEIGH-TAIL-PDF 0.25d0 -2.0d0 2.0d0))))
