;********************************************************
; file:        logarithmic.lisp                          
; description: Logarithmic distribution
; date:        Sat Nov 25 2006 - 16:00
; author:      Liam M. Healy                             
; modified:    Sat Nov 25 2006 - 22:57
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl logarithmic (generator p)
  "gsl_ran_logarithmic"
  (((generator generator) :pointer) (p :double))
  :c-return :uint
  :documentation
  "A random integer from the logarithmic distribution.
   The probability distribution for logarithmic random variates
   is p(k) = {-1 \over \log(1-p)} {\left( p^k \over k \right)}
   for @math{k >= 1}.")

(defun-gsl logarithmic-pdf (k p)
  "gsl_ran_logarithmic_pdf" ((k :uint) (p :double))
  :c-return :double
  :documentation
  "The probability @math{p(k)} of obtaining @var{k}
   from a logarithmic distribution with probability parameter @var{p},
   using the formula given in #'logarithmic.")

;;; Examples and unit test
(lisp-unit:define-test logarithmic
  (lisp-unit:assert-equal
   '(1 3 1 4 1 1 2 1 1 5 2)
   (progn
     (rng-set *rng-mt19937* 0)
     (loop for i from 0 to 10
	collect
	(logarithmic *rng-mt19937* 0.9d0))))
  (lisp-unit:assert-first-fp-equal
   "0.156609215118d+00"
   (logarithmic-pdf 2 0.4d0)))
