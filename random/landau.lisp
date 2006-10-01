;********************************************************
; file:        landau.lisp                          
; description: Landau distribution                  
; date:        Sat Sep 30 2006
; author:      Liam M. Healy                             
; modified:    Sat Sep 30 2006 - 21:52
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl landau (generator)
  "gsl_ran_landau"
  (((generator generator) :pointer))
  :c-return :double
  :documentation
  "A random variate from the Landau distribution.  The
   probability distribution for Landau random variates is defined
   analytically by the complex integral,
   {1 \over {2 \pi i}} \int_{c-i\infty}^{c+i\infty} ds\, \exp(s \log(s) + x s) 
   For numerical purposes it is more convenient to use the following
   equivalent form of the integral,
   p(x) = (1/\pi) \int_0^\infty dt \exp(-t \log(t) - x t) \sin(\pi t).")

(defun-gsl landau-pdf (x)
  "gsl_ran_landau_pdf" ((x :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for the Landau distribution using an approximation to the formula given
   in #'landau.")

;;; Examples and unit test
(lisp-unit:define-test landau
  (lisp-unit:assert-equal
   '("0.388003742625d+04" "-0.695320031455d+00" "-0.235436464660d-01"
     "0.213292096300d+02" "-0.306222470471d+00" "0.124241866694d+01"
     "0.261461684796d+02" "0.433721764097d+01" "0.167995462811d+01"
     "0.424757192183d+01" "0.468150620898d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (landau *rng-mt19937*)))))
  (lisp-unit:assert-first-fp-equal
   "0.173319689959d+00"
   (landau-pdf 0.25d0)))
