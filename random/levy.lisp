;********************************************************
; file:        levy.lisp                          
; description: Levy distribution                  
; date:        Sat Sep 30 2006
; author:      Liam M. Healy                             
; modified:    Sat Sep 30 2006 - 22:45
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl levy (generator c alpha)
  "gsl_ran_levy"
  (((generator generator) :pointer) (c :double) (alpha :double))
  :c-return :double
  :documentation
  "A random variate from the Levy symmetric stable
   distribution with scale @var{c} and exponent @var{alpha}.  The symmetric
   stable probability distribution is defined by a fourier transform
   p(x) = {1 \over 2 \pi} \int_{-\infty}^{+\infty} dt \exp(-it x - |c t|^\alpha)
   There is no explicit solution for the form of @math{p(x)} and the
   library does not define a corresponding @code{pdf} function.  For
   @math{\alpha = 1} the distribution reduces to the Cauchy distribution.  For
   @math{\alpha = 2} it is a Gaussian distribution with @c{$\sigma = \sqrt{2} c$} 
   @math{\sigma = \sqrt@{2@} c}.  For @math{\alpha < 1} the tails of the
   distribution become extremely wide.
   The algorithm only works for @c{$0 < \alpha \le 2$} @math{0 < alpha <= 2}.")

(defun-gsl levy-skew (generator c alpha beta)
  "gsl_ran_levy_skew"
  (((generator generator) :pointer) (c :double) (alpha :double) (beta :double))
  :c-return :double
  :documentation
  "A random variate from the Levy skew stable
   distribution with scale @var{c}, exponent @var{alpha} and skewness
   parameter @var{beta}.  The skewness parameter must lie in the range
   @math{[-1,1]}.  The Levy skew stable probability distribution is defined
   by a fourier transform,
   p(x) = {1 \over 2 \pi} \int_{-\infty}^{+\infty} dt \exp(-it x - |c t|^\alpha (1-i \beta \sign(t) \tan(\pi\alpha/2)))
   When @math{\alpha = 1} the term @math{\tan(\pi \alpha/2)} is replaced by
   @math{-(2/\pi)\log|t|}.  There is no explicit solution for the form of
   @math{p(x)} and the library does not define a corresponding @code{pdf}
   function.  For @math{\alpha = 2} the distribution reduces to a Gaussian
   distribution with @c{$\sigma = \sqrt{2} c$} 
   @math{\sigma = \sqrt@{2@} c} and the skewness parameter has no effect.  
   For @math{\alpha < 1} the tails of the distribution become extremely
   wide.  The symmetric distribution corresponds to @math{\beta = 0}.
   The algorithm only works for @c{$0 < \alpha \le 2$} @math{0 < alpha <= 2}.")


;;; Examples and unit test
(lisp-unit:define-test levy
  (lisp-unit:assert-equal
   '("0.269410983324d+01" "-0.293954386447d+00" "-0.127034013523d+01"
     "0.107715386401d+01" "0.137712184069d+00" "0.941972843811d+00"
     "-0.510713467479d+00" "0.164820785369d+00" "-0.148578990410d+00"
     "-0.190748857444d+01" "-0.208619521400d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (levy *rng-mt19937* 1.0d0 2.0d0)))))
  (lisp-unit:assert-equal
   '("0.269410983324d+01" "-0.293954386447d+00" "-0.127034013523d+01"
     "0.107715386401d+01" "0.137712184069d+00" "0.941972843811d+00"
     "-0.510713467479d+00" "0.164820785369d+00" "-0.148578990410d+00"
     "-0.190748857444d+01" "-0.208619521400d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (levy-skew *rng-mt19937* 1.0d0 2.0d0 1.0d0))))))
