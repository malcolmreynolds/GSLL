;; Exponential distribution
;; Liam Healy, Sun Sep 17 2006
;; Time-stamp: <2008-02-17 12:50:28EST laplace.lisp>
;; $Id$

(in-package :gsl)

(defmfun laplace (generator a)
  "gsl_ran_laplace"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Laplace distribution with width a.
   The distribution is
   p(x) dx = {1 \over 2 a}  \exp(-|x/a|) dx
   for -\infty < x < \infty.")

(defmfun laplace-pdf (x a)
  "gsl_ran_laplace_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Laplace distribution with width a, using the formula
   given for #'laplace.")

(defmfun laplace-P (x a)
  "gsl_cdf_laplace_P" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
   P(x) for the laplace distribution with width a.")

(defmfun laplace-Q (x a)
  "gsl_cdf_laplace_Q" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
   Q(x) for the laplace distribution with width a.")

(defmfun laplace-Pinv (P a)
  "gsl_cdf_laplace_Pinv" ((P :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function
   P(x) for the laplace distribution with width a.")

(defmfun laplace-Qinv (Q a)
  "gsl_cdf_laplace_Qinv" ((Q :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution function
   Q(x) for the laplace distribution with width a.")

;;; Examples and unit test
#|
(make-tests laplace
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (laplace rng 10.0d0)))
  (laplace-pdf 0.0d0 10.0d0)
  (laplace-p 1.0d0 2.0d0)
  (laplace-q 1.0d0 2.0d0)
  (laplace-pinv 0.6967346701436833d0 2.0d0)
  (laplace-qinv 0.3032653298563167d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST LAPLACE
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.005166356198580803d0 -3.942577717493807d0
	  -8.329510281601332d0 1.1159975704649974d0
	  -6.2234038148786555d0 -35.04800398421181d0
	  0.8888158320028845d0 7.161892491969776d0
	  25.24637780914261d0 7.341651048064451d0
	  6.54142651602034d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (LAPLACE RNG 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.05d0)
   (MULTIPLE-VALUE-LIST (LAPLACE-PDF 0.0d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6967346701436833d0)
   (MULTIPLE-VALUE-LIST (LAPLACE-P 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.3032653298563167d0)
   (MULTIPLE-VALUE-LIST (LAPLACE-Q 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0d0)
   (MULTIPLE-VALUE-LIST (LAPLACE-PINV 0.6967346701436833d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0d0)
   (MULTIPLE-VALUE-LIST (LAPLACE-QINV 0.3032653298563167d0 2.0d0))))
