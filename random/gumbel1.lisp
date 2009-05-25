;; The Gumbel type 1 random number distribution
;; Liam Healy, Sun Oct 29 2006
;; Time-stamp: <2009-05-24 21:45:10EDT gumbel1.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h
;;; /usr/include/gsl/gsl_cdf.h

(defmfun sample
    ((generator random-number-generator) (type (eql 'gumbel1)) &key a b)
  "gsl_ran_gumbel1"
  (((mpointer generator) :pointer) (a :double) (b :double))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "A random variate from the Type-1 Gumbel
   distribution,
   p(x) dx = a b \exp(-(b \exp(-ax) + ax)) dx
   for -\infty < x < \infty.")

(defmfun gumbel1-pdf (x a b)
  "gsl_ran_gumbel1_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
  for a Type-1 Gumbel distribution with parameters a and b,
  using the formula given for #'gumbel1.")

(defmfun gumbel1-P (x a b)
  "gsl_cdf_gumbel1_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the Type-1 Gumbel distribution with
  parameters a and b.")

(defmfun gumbel1-Q (x a b)
  "gsl_cdf_gumbel1_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the Type-1 Gumbel distribution with
  parameters a and b.")

(defmfun gumbel1-Pinv (P a b)
  "gsl_cdf_gumbel1_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the Type-1 Gumbel distribution with
  parameters a and b.")

(defmfun gumbel1-Qinv (Q a b)
  "gsl_cdf_gumbel1_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  Q(x) for the Type-1 Gumbel distribution with
  parameters a and b.")

;;; Examples and unit test
(save-test gumbel1
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect (sample rng 'gumbel1 :a 1.0d0 :b 2.0d0)))
  (gumbel1-pdf 0.1d0 1.0d0 2.0d0)
  (gumbel1-P 0.1d0 1.0d0 2.0d0)
  (gumbel1-Q 0.1d0 1.0d0 2.0d0)
  (gumbel1-Pinv 0.1637073598773166d0 1.0d0 2.0d0)
  (gumbel1-Qinv 0.8362926401226833d0 1.0d0 2.0d0))


