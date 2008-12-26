;; Gaussian tail distribution
;; Liam Healy, Mon Aug 21 2006 - 21:52
;; Time-stamp: <2008-12-26 11:45:11EST gaussian-tail.lisp>
;; $Id$

(in-package :gsl)

(defmfun gaussian-tail (generator a sigma)
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

(defmfun gaussian-tail-pdf (x a sigma)
  "gsl_ran_gaussian_tail_pdf" ((x :double) (a :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
  for a Gaussian tail distribution with standard deviation sigma and
  lower limit a, using the formula given for gaussian-tail.")

(defmfun ugaussian-tail (generator a)
  "gsl_ran_ugaussian_tail"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation			; FDL
  "Equivalent to gaussian-tail with sigma=1.")

(defmfun ugaussian-tail-pdf (x a)
  "gsl_ran_ugaussian_tail_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "Equivalent to gaussian-tail-pdf with sigma=1.")

;;; Examples and unit test
(save-test
 gaussian-tail
 (let ((rng (make-random-number-generator *mt19937* 0)))
   (loop for i from 0 to 10
	 collect
	 (gaussian-tail rng 50.0d0 10.0d0)))
 (gaussian-tail-pdf 52.0d0 50.0d0 10.0d0)
 (let ((rng (make-random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (ugaussian-tail rng 5.0d0)))
 (ugaussian-tail-pdf 5.2d0 5.0d0))
