;; Gaussian tail distribution
;; Liam Healy, Mon Aug 21 2006 - 21:52
;; Time-stamp: <2008-02-03 10:15:01EST gaussian-tail.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl gaussian-tail (generator a sigma)
  "gsl_ran_gaussian_tail"
  (((generator generator) :pointer) (a :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "Random variates from the upper tail of a Gaussian
   distribution with standard deviation sigma.  The values returned
   are larger than the lower limit a, which must be positive.  The
   method is based on Marsaglia's famous rectangle-wedge-tail algorithm (Ann. 
   Math. Stat. 32, 894--899 (1961)), with this aspect explained in Knuth, v2,
   3rd ed, p139,586 (exercise 11).
   The probability distribution for Gaussian tail random variates is,
   p(x) dx = {1 \over N(a;\sigma) \sqrt{2 \pi \sigma^2}}
                  \exp (- x^2 / 2\sigma^2) dx
   for x > a where N(a;\sigma) is the normalization constant,
   N(a;\sigma) = (1/2) erfc(a / sqrt(2 sigma^2)).")

(defun-gsl gaussian-tail-pdf (x a sigma)
  "gsl_ran_gaussian_tail_pdf" ((x :double) (a :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
  for a Gaussian tail distribution with standard deviation sigma and
  lower limit a, using the formula given for gaussian-tail.")

(defun-gsl ugaussian-tail (generator a)
  "gsl_ran_ugaussian_tail"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation			; FDL
  "Equivalent to gaussian-tail with sigma=1.")

(defun-gsl ugaussian-tail-pdf (x a)
  "gsl_ran_ugaussian_tail_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "Equivalent to gaussian-tail-pdf with sigma=1.")

;;; Examples and unit test
(lisp-unit:define-test gaussian-tail
  (lisp-unit:assert-equal
   '("0.501083703038d+02" "0.514269594531d+02" "0.505871602700d+02"
     "0.505987522244d+02" "0.508283057286d+02" "0.504334311213d+02"
     "0.534422862873d+02" "0.518376171418d+02" "0.530010742143d+02"
     "0.521497741699d+02" "0.501157244350d+02")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (gaussian-tail rng 50.0d0 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.187022708773d+00"
   (gaussian-tail-pdf 52.0d0 50.0d0 10.0d0))
  (lisp-unit:assert-equal
   '("0.501083703038d+01" "0.514269594531d+01" "0.505871602700d+01"
     "0.505987522244d+01" "0.508283057286d+01" "0.504334311213d+01"
     "0.534422862873d+01" "0.518376171418d+01" "0.530010742143d+01"
     "0.521497741699d+01" "0.501157244350d+01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (ugaussian-tail rng 5.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.187022708773d+01"
   (ugaussian-tail-pdf 5.2d0 5.0d0)))
