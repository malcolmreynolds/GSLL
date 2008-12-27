;; Hypergeometric distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-12-26 19:42:50EST hypergeometric.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h
;;; /usr/include/gsl/gsl_cdf.h

(defmfun hypergeometric (generator n1 n2 tt)
  "gsl_ran_hypergeometric"
  (((mpointer generator) :pointer) (n1 :uint) (n2 :uint)(tt :uint))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the hypergeometric
   distribution.  The probability distribution for hypergeometric
   random variates is
   p(k) =  C(n_1, k) C(n_2, t - k) / C(n_1 + n_2, t)
   where C(a,b) = a!/(b!(a-b)!) and 
   t <= n_1 + n_2.  The domain of k is 
   max(0,t-n_2), ..., min(t,n_1).
   If a population contains n_1 elements of ``type 1'' and
   n_2 elements of ``type 2'' then the hypergeometric
   distribution gives the probability of obtaining k elements of
   ``type 1'' in t samples from the population without
   replacement.")

(defmfun hypergeometric-pdf (k n1 n2 tt)
  "gsl_ran_hypergeometric_pdf" ((k :uint) (n1 :uint) (n2 :uint)(tt :uint))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a hypergeometric distribution with parameters n1, n2,
   tt, using the formula given in #'hypergeometric.")

(defmfun hypergeometric-P (k n1 n2 tt)
  "gsl_cdf_hypergeometric_P" ((k :uint) (n1 :uint) (n2 :uint)(tt :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions P(k) for the
   hypergeometric distribution with parameters n1, n2 and tt.")

(defmfun hypergeometric-Q  (k n1 n2 tt)
  "gsl_cdf_hypergeometric_Q"  ((k :uint) (n1 :uint) (n2 :uint)(tt :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions Q(k) for the
   hypergeometric distribution with parameters n1, n2, and tt.")

;;; Examples and unit test
(save-test hypergeometric-randist
  (let ((rng (make-random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (hypergeometric rng 3 6 3)))
  (hypergeometric-pdf 0 2 6 3)
  (hypergeometric-P 1 2 6 3)
  (hypergeometric-Q 1 2 6 3))


