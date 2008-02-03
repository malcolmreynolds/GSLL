;; Levy distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-03 10:44:49EST levy.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl levy (generator c alpha)
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

(defun-gsl levy-skew (generator c alpha beta)
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
(lisp-unit:define-test levy
  (lisp-unit:assert-equal
   '("0.269410983324d+01" "-0.293954386447d+00" "-0.127034013523d+01"
     "0.107715386401d+01" "0.137712184069d+00" "0.941972843811d+00"
     "-0.510713467479d+00" "0.164820785369d+00" "-0.148578990410d+00"
     "-0.190748857444d+01" "-0.208619521400d+01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (levy rng 1.0d0 2.0d0)))))
  (lisp-unit:assert-equal
   '("0.269410983324d+01" "-0.293954386447d+00" "-0.127034013523d+01"
     "0.107715386401d+01" "0.137712184069d+00" "0.941972843811d+00"
     "-0.510713467479d+00" "0.164820785369d+00" "-0.148578990410d+00"
     "-0.190748857444d+01" "-0.208619521400d+01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (levy-skew rng 1.0d0 2.0d0 1.0d0))))))
