;; The Gumbel type 1 random number distribution
;; Liam Healy, Sun Oct 29 2006
;; Time-stamp: <2008-02-03 09:46:20EST gumbel1.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl gumbel1 (generator a b)
  "gsl_ran_gumbel1"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Type-1 Gumbel
   distribution,
   p(x) dx = a b \exp(-(b \exp(-ax) + ax)) dx
   for -\infty < x < \infty.")

(defun-gsl gumbel1-pdf (x a b)
  "gsl_ran_gumbel1_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
  for a Type-1 Gumbel distribution with parameters a and b,
  using the formula given for #'gumbel1.")

(defun-gsl gumbel1-P (x a b)
  "gsl_cdf_gumbel1_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the Type-1 Gumbel distribution with
  parameters a and b.")

(defun-gsl gumbel1-Q (x a b)
  "gsl_cdf_gumbel1_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the Type-1 Gumbel distribution with
  parameters a and b.")

(defun-gsl gumbel1-Pinv (P a b)
  "gsl_cdf_gumbel1_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the Type-1 Gumbel distribution with
  parameters a and b.")

(defun-gsl gumbel1-Qinv (Q a b)
  "gsl_cdf_gumbel1_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  Q(x) for the Type-1 Gumbel distribution with
  parameters a and b.")

;;; Examples and unit test
(lisp-unit:define-test gumbel1
  (lisp-unit:assert-equal   
   '("0.895459625749d+01" "0.973051899751d-01" "0.459135062331d+00"
     "0.360741242243d+01" "0.313000274682d+00" "0.101657969497d+01"
     "0.382920819366d+01" "0.191289739318d+01" "0.117748457895d+01"
     "0.189323210797d+01" "0.198591186168d+01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect (gumbel1 rng 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.296257089650d+00"
   (gumbel1-pdf 0.1d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.163707359877d+00"
   (gumbel1-P 0.1d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.836292640123d+00"
   (gumbel1-Q 0.1d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+00"
   (gumbel1-Pinv 0.1637073598773166d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+00"
   (gumbel1-Qinv 0.8362926401226833d0 1.0d0 2.0d0)))
