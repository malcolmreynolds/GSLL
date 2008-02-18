;; Beta distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 13:18:37EST beta.lisp>
;; $Id: $

(in-package :gsl)

(defmfun beta-rd (generator a b)
  ;; Named #'beta-rd to avoid confusion with the special function #'beta.
  "gsl_ran_beta"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the beta distribution.  The distribution function is
   p(x) dx = {\Gamma(a+b) \over \Gamma(a) \Gamma(b)} x^{a-1} (1-x)^{b-1} dx
   0 <= x <= 1.")

(defmfun beta-pdf (x a b)
  "gsl_ran_beta_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a beta distribution with parameters a and b, using the
   formula given in #'beta.")

(defmfun beta-P (x a b)
  "gsl_cdf_beta_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the beta distribution with parameters a and b.")

(defmfun beta-Q (x a b)
  "gsl_cdf_beta_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the beta distribution with parameters a and b.")

(defmfun beta-Pinv (P a b)
  "gsl_cdf_beta_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the beta distribution with parameters a and b.")

(defmfun beta-Qinv (Q a b)
  "gsl_cdf_beta_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the beta distribution with parameters a and b.")

;;; Examples and unit test
#|
(make-tests beta
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (beta-rd rng 1.0d0 2.0d0)))
  (beta-pdf 0.1d0 1.0d0 2.0d0)
  (beta-P 0.1d0 1.0d0 2.0d0)
  (beta-Q 0.1d0 1.0d0 2.0d0)
  (beta-Pinv 0.19d0 1.0d0 2.0d0)
  (beta-Qinv 0.81d0 1.0d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST BETA
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 8.390009419017379d-5 0.024211646813900284d0
	  0.045507713472575685d0 0.30321144534029304d0
	  0.5693572151110255d0 0.5146515206667992d0
	  0.230096194772543d0 0.3928348825648452d0
	  0.5143874122537551d0 0.2337836858050161d0
	  0.19851288668620246d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (BETA-RD RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.8000000000000016d0)
   (MULTIPLE-VALUE-LIST (BETA-PDF 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.19000000000000017d0)
   (MULTIPLE-VALUE-LIST (BETA-P 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8099999999999998d0)
   (MULTIPLE-VALUE-LIST (BETA-Q 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.09999999999999988d0)
   (MULTIPLE-VALUE-LIST (BETA-PINV 0.19d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.09999999999999987d0)
   (MULTIPLE-VALUE-LIST (BETA-QINV 0.81d0 1.0d0 2.0d0))))

