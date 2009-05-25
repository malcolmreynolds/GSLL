;; Fdist distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2009-05-24 20:24:40EDT fdist.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h
;;; /usr/include/gsl/gsl_cdf.h

(defmfun sample
    ((generator random-number-generator) (type (eql 'fdist)) &key nu1 nu2)
  "gsl_ran_fdist"
  (((mpointer generator) :pointer) (nu1 :double) (nu2 :double))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "A random variate from the F-distribution with degrees of freedom nu1
   and nu2.  The distribution function is
   p(x) dx = 
   { \Gamma((\nu_1 + \nu_2)/2)
        \over \Gamma(\nu_1/2) \Gamma(\nu_2/2) } 
   \nu_1^{\nu_1/2} \nu_2^{\nu_2/2} 
   x^{\nu_1/2 - 1} (\nu_2 + \nu_1 x)^{-\nu_1/2 -\nu_2/2}
   for x >= 0.")

(defmfun fdist-pdf (x nu1 nu2)
  "gsl_ran_fdist_pdf" ((x :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for an F-distribution with nu1 and nu2 degrees of freedom,
   using the formula given #'fdist.")

(defmfun fdist-P (x nu1 nu2)
  "gsl_cdf_fdist_P" ((x :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the fdist distribution with
  nu1 and nu2 degrees of freedom.")

(defmfun fdist-Q (x nu1 nu2)
  "gsl_cdf_fdist_Q" ((x :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the fdist distribution with
  nu1 and nu2 degrees of freedom.")

(defmfun fdist-Pinv (P nu1 nu2)
  "gsl_cdf_fdist_Pinv" ((P :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the fdist distribution with
  nu1 and nu2 degrees of freedom.")

(defmfun fdist-Qinv (Q nu1 nu2)
  "gsl_cdf_fdist_Qinv" ((Q :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the fdist distribution with
   nu1 and nu2 degrees of freedom.")

;;; Examples and unit test
(save-test fdist
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect
	    (sample rng 'fdist :nu1 1.0d0 :nu2 2.0d0)))
  (fdist-pdf 1.2d0 1.0d0 2.0d0)
  (fdist-P 1.2d0 1.0d0 2.0d0)
  (fdist-Q 1.2d0 1.0d0 2.0d0)
  (fdist-Pinv 0.612372435695795d0 1.0d0 2.0d0)
  (fdist-Qinv 0.38762756430420503d0 1.0d0 2.0d0))
