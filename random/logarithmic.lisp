;; Logarithmic distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2009-05-24 22:49:00EDT logarithmic.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h

(defmfun sample
    ((generator random-number-generator) (type (eql 'logarithmic))
     &key probability)
  "gsl_ran_logarithmic"
  (((mpointer generator) :pointer) (probability :double))
  :definition :method
  :c-return :uint
  :documentation			; FDL
  "A random integer from the logarithmic distribution.
   The probability distribution for logarithmic random variates
   is p(k) = {-1 \over \log(1-p)} {\left( p^k \over k \right)}
   for k >= 1.")

(defmfun logarithmic-pdf (k p)
  "gsl_ran_logarithmic_pdf" ((k :uint) (p :double))
  :c-return :double
  :documentation
  "The probability p(k) of obtaining k
   from a logarithmic distribution with probability parameter p,
   using the formula given in #'logarithmic.")

;;; Examples and unit test
(save-test logarithmic
  (let ((rng (make-random-number-generator +mt19937+ 0)))
     (loop for i from 0 to 10
	   collect
	   (sample rng 'logarithmic :probability 0.9d0)))
  (logarithmic-pdf 2 0.4d0))
