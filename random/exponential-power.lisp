;; Exponential power distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-03 11:12:27EST exponential-power.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl exponential-power (generator a b)
  "gsl_ran_exppow"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the exponential power distribution
   with scale parameter a and exponent b.  The distribution is
   p(x) dx = {1 \over 2 a \Gamma(1+1/b)} \exp(-|x/a|^b) dx
   for x >= 0.  For b = 1 this reduces to the Laplace
   distribution.  For b = 2 it has the same form as a gaussian
   distribution, but with a = \sqrt{2} \sigma.")

(defun-gsl exponential-power-pdf (x a b)
  "gsl_ran_exppow_pdf" 
  ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for an exponential power distribution with scale parameter a
   and exponent b, using the formula given for #'exponential-power.")

(defun-gsl exponential-power-P (x a b)
  "gsl_cdf_exppow_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
  P(x), for the exponential power distribution with
  parameters a and b.")

(defun-gsl exponential-power-Q (x a b)
  "gsl_cdf_exppow_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions Q(x)
  for the exponential power distribution with
  parameters a and b.")

;;; Examples and unit test
(lisp-unit:define-test exponential-power
  (lisp-unit:assert-equal
   '("0.946947559278d-01" "-0.622968087533d-01" "0.118398553854d+01"
     "0.518762601924d+00" "0.705356431406d+00" "-0.903330384457d+00"
     "-0.169473362899d+01" "-0.480323610806d+00" "-0.276417363499d-01"
     "0.631839185605d+00" "-0.124788752274d-01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (exponential-power rng 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.564189583548d+00"
   (exponential-power-pdf 0.0d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.921350396475d+00"
   (exponential-power-P 1.0d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.786496035251d-01"
   (exponential-power-Q 1.0d0 1.0d0 2.0d0)))
