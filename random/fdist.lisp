;; Fdist distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 13:14:44EST fdist.lisp>
;; $Id$

(in-package :gsl)

(defmfun fdist (generator nu1 nu2)
  "gsl_ran_fdist"
  (((generator generator) :pointer) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the F-distribution with degrees of freedom nu1
   and nu2.  The distribution function is
   p(x) dx = 
   { \Gamma((\nu_1 + \nu_2)/2)
        \over \Gamma(\nu_1/2) \Gamma(\nu_2/2) } 
   \nu_1^{\nu_1/2} \nu_2^{\nu_2/2} 
   x^{\nu_1/2 - 1} (\nu_2 + \nu_1 x)^{-\nu_1/2 -\nu_2/2}
   for x >= 0.")

(defmfun fdist-pdf (x nu1 nu2)
  "gsl_ran_fdist_pdf" ((x :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for an F-distribution with nu1 and nu2 degrees of freedom,
   using the formula given #'fdist.")

(defmfun fdist-P (x nu1 nu2)
  "gsl_cdf_fdist_P" ((x :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the fdist distribution with
  nu1 and nu2 degrees of freedom.")

(defmfun fdist-Q (x nu1 nu2)
  "gsl_cdf_fdist_Q" ((x :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the fdist distribution with
  nu1 and nu2 degrees of freedom.")

(defmfun fdist-Pinv (P nu1 nu2)
  "gsl_cdf_fdist_Pinv" ((P :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the fdist distribution with
  nu1 and nu2 degrees of freedom.")

(defmfun fdist-Qinv (Q nu1 nu2)
  "gsl_cdf_fdist_Qinv" ((Q :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the fdist distribution with
   nu1 and nu2 degrees of freedom.")

;;; Examples and unit test
#|
(make-tests fdist
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (fdist rng 1.0d0 2.0d0)))
  (fdist-pdf 1.2d0 1.0d0 2.0d0)
  (fdist-P 1.2d0 1.0d0 2.0d0)
  (fdist-Q 1.2d0 1.0d0 2.0d0)
  (fdist-Pinv 0.612372435695795d0 1.0d0 2.0d0)
  (fdist-Qinv 0.38762756430420503d0 1.0d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST FDIST
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 103.77423336538772d0 2.124850132212401d0
	  0.3044106947086728d0 0.30018868738752463d0
	  0.0011228206844788875d0 0.29210940078495257d0
	  0.0635729092565007d0 0.4779663652167387d0
	  0.03472116760794376d0 0.4869748230412714d0
	  2.531794516961295d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (FDIST RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.1594719884624466d0)
   (MULTIPLE-VALUE-LIST (FDIST-PDF 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6123724356957948d0)
   (MULTIPLE-VALUE-LIST (FDIST-P 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.3876275643042052d0)
   (MULTIPLE-VALUE-LIST (FDIST-Q 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.200000000000001d0)
   (MULTIPLE-VALUE-LIST
    (FDIST-PINV 0.612372435695795d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.2000000000000006d0)
   (MULTIPLE-VALUE-LIST
    (FDIST-QINV 0.38762756430420503d0 1.0d0 2.0d0))))

