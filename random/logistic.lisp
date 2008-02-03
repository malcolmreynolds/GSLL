;; Logistic distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2008-02-03 10:35:08EST logistic.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl logistic (generator a)
  "gsl_ran_logistic"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the logistic distribution.  The distribution function is
   p(x) dx = { \exp(-x/a) \over a (1 + \exp(-x/a))^2 } dx
   for -\infty < x < +\infty.")

(defun-gsl logistic-pdf (x a)
  "gsl_ran_logistic_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a logistic distribution with scale parameter a, using the
   formula given in #'logistic.")

(defun-gsl logistic-P (x a)
  "gsl_cdf_logistic_P" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the logistic distribution with scale parameter a.")

(defun-gsl logistic-Q (x a)
  "gsl_cdf_logistic_Q" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the logistic distribution with scale parameter a.")

(defun-gsl logistic-Pinv (P a)
  "gsl_cdf_logistic_Pinv" ((P :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the logistic distribution with scale parameter a.")

(defun-gsl logistic-Qinv (Q a)
  "gsl_cdf_logistic_Qinv" ((Q :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the logistic distribution with scale parameter a.")

;;; Examples and unit test
(lisp-unit:define-test logistic
  (lisp-unit:assert-equal
   '("0.826131993192d+02" "-0.163673460427d+02" "-0.931513272044d+01"
     "0.288702070871d+02" "-0.119898098758d+02" "-0.601236476200d+00"
     "0.311425552636d+02" "0.106846737210d+02" "0.160518409540d+01"
     "0.104572419047d+02" "0.115237141063d+02")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (logistic rng 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.235003712202d+00"
   (logistic-pdf 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.622459331202d+00"
   (logistic-P 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.377540668798d+00"
   (logistic-Q 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d+00"
   (logistic-Pinv 0.6224593312018546d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d+00"
   (logistic-Qinv 0.37754066879814546d0 1.0d0)))
