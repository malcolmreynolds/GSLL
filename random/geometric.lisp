;; Geometric distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2009-05-24 22:28:31EDT geometric.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h

(defmfun sample
    ((generator random-number-generator) (type (eql 'geometric))
     &key probability)
  "gsl_ran_geometric"
  (((mpointer generator) :pointer) (probability :double))
  :definition :method
  :c-return :uint
  :documentation			; FDL
  "A random integer from the geometric distribution,
   the number of independent trials with probability p until the
   first success.  The probability distribution for geometric variates
   is p(k) =  p (1-p)^{k-1} for k >= 1.
   Note that the distribution begins with k=1 with this
   definition.  There is another convention in which the exponent k-1
   is replaced by k.")

(defmfun geometric-pdf (k p)
  "gsl_ran_geometric_pdf" ((k :uint) (p :double))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a geometric distribution with probability parameter p, using
   the formula given in #'geometric.")

(defmfun geometric-P (k p)
  "gsl_cdf_geometric_P" ((k :uint) (p :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the geometric distribution with parameter p.")

(defmfun geometric-Q (k p)
  "gsl_cdf_geometric_Q" ((k :uint) (p :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(k) for the geometric distribution with parameters p.")

;;; Examples and unit test
(save-test geometric
  (let ((rng (make-random-number-generator +mt19937+ 0)))
     (loop for i from 0 to 10
	   collect
	   (sample rng 'geometric :probability 0.4d0)))
  (geometric-pdf 2 0.4d0)
  (geometric-P 2 0.4d0)
  (geometric-Q 2 0.4d0))
