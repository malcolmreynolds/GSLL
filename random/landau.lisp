;; Landau distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2009-05-24 20:08:47EDT landau.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h

(defmfun sample
    ((generator random-number-generator) (type (eql 'landau)) &key)
  "gsl_ran_landau"
  (((mpointer generator) :pointer))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "A random variate from the Landau distribution.  The
   probability distribution for Landau random variates is defined
   analytically by the complex integral,
   {1 \over {2 \pi i}} \int_{c-i\infty}^{c+i\infty} ds\, \exp(s \log(s) + x s) 
   For numerical purposes it is more convenient to use the following
   equivalent form of the integral,
   p(x) = (1/\pi) \int_0^\infty dt \exp(-t \log(t) - x t) \sin(\pi t).")

(defmfun landau-pdf (x)
  "gsl_ran_landau_pdf" ((x :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for the Landau distribution using an approximation to the formula given
   in #'landau.")

;;; Examples and unit test
(save-test landau
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect (sample rng 'landau)))
  (landau-pdf 0.25d0))
