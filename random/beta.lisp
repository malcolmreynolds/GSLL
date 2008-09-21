;; Beta distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-09-20 21:20:32EDT beta.lisp>
;; $Id$

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
    (LIST 0.3935474359990073 0.7063621551518341
	  0.044515648447265056 0.09286083229785232
	  0.210544366728104 0.010114317425185686
	  0.4595767375719009 0.1515157002550483
	  0.1731331145031117 0.4270743075655188
	  0.3353314142479658))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (BETA-RD RNG 1.0 2.0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.8000000000000016)
   (MULTIPLE-VALUE-LIST (BETA-PDF 0.1 1.0 2.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.19000000000000017)
   (MULTIPLE-VALUE-LIST (BETA-P 0.1 1.0 2.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8099999999999998)
   (MULTIPLE-VALUE-LIST (BETA-Q 0.1 1.0 2.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.09999999999999992)
   (MULTIPLE-VALUE-LIST (BETA-PINV 0.19 1.0 2.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.09999999999999988)
   (MULTIPLE-VALUE-LIST (BETA-QINV 0.81 1.0 2.0))))


