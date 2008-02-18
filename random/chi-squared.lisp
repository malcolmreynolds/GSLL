;; Chi-squared distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2008-02-17 13:12:43EST chi-squared.lisp>
;; $Id: $

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
    (LIST 9.190439057230117d0 3.9593845312309113d0
	  5.434933385075541d0 13.023107419896268d0
	  13.670690841729192d0 6.982359715352029d0
	  13.75663972164132d0 11.359458045417648d0
	  16.06828259064875d0 12.999514252538129d0
	  11.147347424355443d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (CHI-SQUARED RNG 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.43939128946772227d0)
   (MULTIPLE-VALUE-LIST (CHI-SQUARED-PDF 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5204998778130462d0)
   (MULTIPLE-VALUE-LIST (CHI-SQUARED-P 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4795001221869538d0)
   (MULTIPLE-VALUE-LIST (CHI-SQUARED-Q 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5000000000000003d0)
   (MULTIPLE-VALUE-LIST
    (CHI-SQUARED-PINV 0.5204998778130463d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5000000000000003d0)
   (MULTIPLE-VALUE-LIST
    (CHI-SQUARED-QINV 0.4795001221869537d0 1.0d0))))

