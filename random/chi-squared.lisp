;; Chi-squared distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2008-09-20 21:23:34EDT chi-squared.lisp>
;; $Id$

(in-package :gsl)

(defmfun chi-squared (generator nu)
  "gsl_ran_chisq"
  (((generator generator) :pointer) (nu :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the chi-squared distribution
  with nu degrees of freedom. The distribution function is
  p(x) dx = {1 \over 2 \Gamma(\nu/2) } (x/2)^{\nu/2 - 1} \exp(-x/2) dx
  x >= 0. ")

(defmfun chi-squared-pdf (x nu)
  "gsl_ran_chisq_pdf" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a chi-squared distribution with nu degrees of freedom, using
   the formula given in #'chi-squared.")

(defmfun chi-squared-P (x nu)
  "gsl_cdf_chisq_P" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the chi-squared distribution with nu degrees of freedom.")

(defmfun chi-squared-Q (x nu)
  "gsl_cdf_chisq_Q" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the chi-squared distribution with nu degrees of freedom.")

(defmfun chi-squared-Pinv (P nu)
  "gsl_cdf_chisq_Pinv" ((P :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the chi-squared distribution with nu degrees of freedom.")

(defmfun chi-squared-Qinv (Q nu)
  "gsl_cdf_chisq_Qinv" ((Q :double) (nu :double))
  :c-return :double
  :documentation 			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the chi-squared distribution with nu degrees of freedom.")

;;; Examples and unit test
#|
(make-tests chi-squared
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (chi-squared rng 10.0d0)))
  (chi-squared-pdf 0.5d0 1.0d0)
  (chi-squared-P 0.5d0 1.0d0)
  (chi-squared-Q 0.5d0 1.0d0)
  (chi-squared-Pinv 0.5204998778130463d0 1.0d0)
  (chi-squared-Qinv 0.4795001221869537d0 1.0d0))
|#

(LISP-UNIT:DEFINE-TEST CHI-SQUARED
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 13.043328884186328 11.427227829236712
	  16.55811815484942 7.128795406995407
	  5.120266499239882 10.464572605669142
	  5.8126929867006405 8.784940866479005
	  7.559275305609187 8.35181083950897
	  4.140798004825149))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (CHI-SQUARED RNG 10.0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.43939128946772227)
   (MULTIPLE-VALUE-LIST (CHI-SQUARED-PDF 0.5 1.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5204998778130462)
   (MULTIPLE-VALUE-LIST (CHI-SQUARED-P 0.5 1.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4795001221869538)
   (MULTIPLE-VALUE-LIST (CHI-SQUARED-Q 0.5 1.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5000000000000003)
   (MULTIPLE-VALUE-LIST
    (CHI-SQUARED-PINV 0.5204998778130463 1.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5000000000000003)
   (MULTIPLE-VALUE-LIST
    (CHI-SQUARED-QINV 0.4795001221869537 1.0))))


