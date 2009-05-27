;; Bernoulli distribution
;; Liam Healy, Sat Nov 25 2006 - 16:59
;; Time-stamp: <2009-05-26 22:56:49EDT bernoulli.lisp>
;; $Id$

(in-package :gsl)

(defmfun sample
    ((generator random-number-generator) (type (eql 'bernoulli))
     &key probability)
  "gsl_ran_bernoulli"
  (((mpointer generator) :pointer) (probability :double))
  :definition :method
  :c-return :uint
  :documentation			; FDL
  "Returns either 0 or 1, the result of a Bernoulli trial
   with probability p.  The probability distribution for
   a Bernoulli trial is
   p(0) = 1 - p
   p(1) = p.")

(defmfun bernoulli-pdf (k p)
  "gsl_ran_bernoulli_pdf" ((k :uint) (p :double))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining
  k from a Bernoulli distribution with probability parameter
  p, using the formula given in #'bernoulli.")

;;; Examples and unit test
(save-test bernoulli
  (let ((rng (make-random-number-generator +mt19937+ 0)))
     (loop for i from 0 to 10
	   collect
	   (sample rng 'bernoulli :probability 0.5d0)))
  (bernoulli-pdf 0 0.5d0))
