;; Levy distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 13:03:36EST levy.lisp>
;; $Id$

(in-package :gsl)

(defmfun levy (generator c alpha)
  "gsl_ran_levy"
  (((generator generator) :pointer) (c :double) (alpha :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Levy symmetric stable
   distribution with scale c and exponent alpha.  The symmetric
   stable probability distribution is defined by a fourier transform,
   p(x) = {1 \over 2 \pi} \int_{-\infty}^{+\infty} dt \exp(-it x - |c t|^\alpha)
   There is no explicit solution for the form of p(x) and the
   library does not define a corresponding pdf function.  For
   \alpha = 1 the distribution reduces to the Cauchy distribution.  For
   \alpha = 2 it is a Gaussian distribution with \sigma = \sqrt{2} c
   For \alpha < 1 the tails of the distribution become extremely wide.
   The algorithm only works for 0 < alpha <= 2.")

(defmfun levy-skew (generator c alpha beta)
  "gsl_ran_levy_skew"
  (((generator generator) :pointer) (c :double) (alpha :double) (beta :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Levy skew stable
   distribution with scale c exponent alpha and skewness
   parameter beta.  The skewness parameter must lie in the range
   [-1,1].  The Levy skew stable probability distribution is defined
   by a fourier transform,
   p(x) = {1 \over 2 \pi} \int_{-\infty}^{+\infty} dt
        \exp(-it x - |c t|^\alpha (1-i \beta \sign(t) \tan(\pi\alpha/2)))
   When \alpha = 1 the term \tan(\pi \alpha/2) is replaced by
   -(2/\pi)\log|t|.  There is no explicit solution for the form of
   p(x)} and the library does not define a corresponding pdf
   function.  For \alpha = 2 the distribution reduces to a Gaussian
   distribution with \sigma = \sqrt{2} c and the skewness parameter
   has no effect.   For \alpha < 1 the tails of the distribution
   become extremely wide.  The symmetric distribution corresponds to \beta = 0.
   The algorithm only works for 0 < \alpha \le 2.")


;;; Examples and unit test
#|
(make-tests levy
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (levy rng 1.0d0 2.0d0)))
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (levy-skew rng 1.0d0 2.0d0 1.0d0))))
|#

(LISP-UNIT:DEFINE-TEST LEVY
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 2.6941098332360465d0 -0.29395438644676647d0
	  -1.2703401352272083d0 1.0771538640113538d0
	  0.13771218406916103d0 0.9419728438107844d0
	  -0.5107134674789159d0 0.1648207853689268d0
	  -0.14857899041035147d0 -1.9074885744364487d0
	  -2.086195213997167d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (LEVY RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 2.6941098332360465d0 -0.2939543864467665d0
	  -1.2703401352272083d0 1.0771538640113538d0
	  0.13771218406916097d0 0.9419728438107844d0
	  -0.510713467478916d0 0.1648207853689266d0
	  -0.14857899041035158d0 -1.907488574436449d0
	  -2.086195213997167d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (LEVY-SKEW RNG 1.0d0 2.0d0 1.0d0))))))

