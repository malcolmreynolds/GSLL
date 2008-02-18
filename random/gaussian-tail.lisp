;; Gaussian tail distribution
;; Liam Healy, Mon Aug 21 2006 - 21:52
;; Time-stamp: <2008-02-17 12:28:20EST gaussian-tail.lisp>
;; $Id: $

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
#|
(make-tests
 gaussian-tail
 (letm ((rng (random-number-generator *mt19937* 0)))
   (loop for i from 0 to 10
	 collect
	 (gaussian-tail rng 50.0d0 10.0d0)))
 (gaussian-tail-pdf 52.0d0 50.0d0 10.0d0)
 (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (ugaussian-tail rng 5.0d0)))
 (ugaussian-tail-pdf 5.2d0 5.0d0))
|#

(LISP-UNIT:DEFINE-TEST GAUSSIAN-TAIL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 50.10837030381376d0 51.42695945309931d0
	  50.587160269982604d0 50.59875222444504d0
	  50.82830572864337d0 50.43343112125345d0
	  53.442286287287374d0 51.83761714183811d0
	  53.00107421429086d0 52.149774169929884d0
	  50.11572443504253d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (GAUSSIAN-TAIL RNG 50.0d0 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.18702270877331703d0)
   (MULTIPLE-VALUE-LIST
    (GAUSSIAN-TAIL-PDF 52.0d0 50.0d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 5.010837030381376d0 5.142695945309931d0
	  5.05871602699826d0 5.0598752224445045d0
	  5.082830572864337d0 5.043343112125345d0
	  5.344228628728738d0 5.183761714183811d0
	  5.300107421429086d0 5.214977416992989d0
	  5.011572443504253d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (UGAUSSIAN-TAIL RNG 5.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.8702270877331704d0)
   (MULTIPLE-VALUE-LIST (UGAUSSIAN-TAIL-PDF 5.2d0 5.0d0))))
