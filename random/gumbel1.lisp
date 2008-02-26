;; The Gumbel type 1 random number distribution
;; Liam Healy, Sun Oct 29 2006
;; Time-stamp: <2008-02-17 13:30:24EST gumbel1.lisp>
;; $Id$

(in-package :gsl)

(defmfun gumbel1 (generator a b)
  "gsl_ran_gumbel1"
  (((generator generator) :pointer) (a :double) (b :double))
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
#|
(make-tests gumbel1
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect (gumbel1 rng 1.0d0 2.0d0)))
  (gumbel1-pdf 0.1d0 1.0d0 2.0d0)
  (gumbel1-P 0.1d0 1.0d0 2.0d0)
  (gumbel1-Q 0.1d0 1.0d0 2.0d0)
  (gumbel1-Pinv 0.1637073598773166d0 1.0d0 2.0d0)
  (gumbel1-Qinv 0.8362926401226833d0 1.0d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST GUMBEL1
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 8.954596257487015d0 0.0973051899750762d0
	  0.45913506233088003d0 3.6074124224293223d0
	  0.31300027468174807d0 1.0165796949651174d0
	  3.8292081936610396d0 1.912897393181305d0
	  1.17748457894919d0 1.893232107970416d0
	  1.9859118616847695d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (GUMBEL1 RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.29625708964974956d0)
   (MULTIPLE-VALUE-LIST (GUMBEL1-PDF 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.1637073598773166d0)
   (MULTIPLE-VALUE-LIST (GUMBEL1-P 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8362926401226833d0)
   (MULTIPLE-VALUE-LIST (GUMBEL1-Q 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.10000000000000007d0)
   (MULTIPLE-VALUE-LIST
    (GUMBEL1-PINV 0.1637073598773166d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.10000000000000028d0)
   (MULTIPLE-VALUE-LIST
    (GUMBEL1-QINV 0.8362926401226833d0 1.0d0 2.0d0))))

