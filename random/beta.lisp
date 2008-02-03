;; Beta distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-02 23:04:13EST beta.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl beta-rd (generator a b)
  ;; Named #'beta-rd to avoid confusion with the special function #'beta.
  "gsl_ran_beta"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation
  "A random variate from the beta distribution.  The distribution function is
   p(x) dx = {\Gamma(a+b) \over \Gamma(a) \Gamma(b)} x^{a-1} (1-x)^{b-1} dx
   @math{0 <= x <= 1}.")

(defun-gsl beta-pdf (x a b)
  "gsl_ran_beta_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a beta distribution with parameters @var{a} and @var{b}, using the
   formula given in #'beta.")

(defun-gsl beta-P (x a b)
  "gsl_cdf_beta_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for the beta distribution with parameters @var{a} and @var{b}.")

(defun-gsl beta-Q (x a b)
  "gsl_cdf_beta_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for the beta distribution with parameters @var{a} and @var{b}.")

(defun-gsl beta-Pinv (P a b)
  "gsl_cdf_beta_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for the beta distribution with parameters @var{a} and @var{b}.")

(defun-gsl beta-Qinv (Q a b)
  "gsl_cdf_beta_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
   @math{Q(x)} for the beta distribution with parameters @var{a} and @var{b}.")

;;; Examples and unit test
(lisp-unit:define-test beta
  (lisp-unit:assert-equal
   '("0.839000941902d-04" "0.242116468139d-01" "0.455077134726d-01"
     "0.303211445340d+00" "0.569357215111d+00" "0.514651520667d+00"
     "0.230096194773d+00" "0.392834882565d+00" "0.514387412254d+00"
     "0.233783685805d+00" "0.198512886686d+00")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (beta-rd rng 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.180000000000d+01"
   (beta-pdf 0.1d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.190000000000d+00"
   (beta-P 0.1d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.810000000000d+00"
   (beta-Q 0.1d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "1.000000000000d-01"
   (beta-Pinv 0.19d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "1.000000000000d-01"
   (beta-Qinv 0.81d0 1.0d0 2.0d0)))
