;; Logistic distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2008-02-17 13:20:11EST logistic.lisp>
;; $Id$

(in-package :gsl)

(defmfun logistic (generator a)
  "gsl_ran_logistic"
  (((generator generator) :pointer) (a :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the logistic distribution.  The distribution function is
   p(x) dx = { \exp(-x/a) \over a (1 + \exp(-x/a))^2 } dx
   for -\infty < x < +\infty.")

(defmfun logistic-pdf (x a)
  "gsl_ran_logistic_pdf" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a logistic distribution with scale parameter a, using the
   formula given in #'logistic.")

(defmfun logistic-P (x a)
  "gsl_cdf_logistic_P" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the logistic distribution with scale parameter a.")

(defmfun logistic-Q (x a)
  "gsl_cdf_logistic_Q" ((x :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the logistic distribution with scale parameter a.")

(defmfun logistic-Pinv (P a)
  "gsl_cdf_logistic_Pinv" ((P :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the logistic distribution with scale parameter a.")

(defmfun logistic-Qinv (Q a)
  "gsl_cdf_logistic_Qinv" ((Q :double) (a :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the logistic distribution with scale parameter a.")

;;; Examples and unit test
#|
(make-tests logistic
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (logistic rng 10.0d0)))
  (logistic-pdf 0.5d0 1.0d0)
  (logistic-P 0.5d0 1.0d0)
  (logistic-Q 0.5d0 1.0d0)
  (logistic-Pinv 0.6224593312018546d0 1.0d0)
  (logistic-Qinv 0.37754066879814546d0 1.0d0))
|#

(LISP-UNIT:DEFINE-TEST LOGISTIC
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 82.6131993192451d0 -16.367346042668906d0
	  -9.31513272043762d0 28.87020708710654d0
	  -11.989809875784625d0 -0.6012364762000397d0
	  31.142555263622d0 10.684673721048895d0
	  1.6051840954024446d0 10.457241904701199d0
	  11.523714106294097d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (LOGISTIC RNG 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.2350037122015945d0)
   (MULTIPLE-VALUE-LIST (LOGISTIC-PDF 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6224593312018546d0)
   (MULTIPLE-VALUE-LIST (LOGISTIC-P 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.37754066879814546d0)
   (MULTIPLE-VALUE-LIST (LOGISTIC-Q 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5000000000000001d0)
   (MULTIPLE-VALUE-LIST
    (LOGISTIC-PINV 0.6224593312018546d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4999999999999998d0)
   (MULTIPLE-VALUE-LIST
    (LOGISTIC-QINV 0.37754066879814546d0 1.0d0))))

