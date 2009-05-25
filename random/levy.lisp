;; Levy distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2009-05-24 20:10:32EDT levy.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h

(defmfun sample
    ((generator random-number-generator) (type (eql 'levy)) &key c alpha)
  "gsl_ran_levy"
  (((mpointer generator) :pointer) (c :double) (alpha :double))
  :definition :method
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

(defmfun sample
    ((generator random-number-generator) (type (eql 'levy-skew))
     &key c alpha beta)
  "gsl_ran_levy_skew"
  (((mpointer generator) :pointer) (c :double) (alpha :double) (beta :double))
  :definition :method
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
(save-test levy
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect
	    (sample rng 'levy :c 1.0d0 :alpha 2.0d0)))
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect
	    (sample rng 'levy-skew :c 1.0d0 :alpha 2.0d0 :beta 1.0d0))))
