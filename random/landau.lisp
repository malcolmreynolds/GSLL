;; Landau distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-17 13:01:50EST landau.lisp>
;; $Id$

(in-package :gsl)

(defmfun landau (generator)
  "gsl_ran_landau"
  (((generator generator) :pointer))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Landau distribution.  The
   probability distribution for Landau random variates is defined
   analytically by the complex integral,
   {1 \over {2 \pi i}} \int_{c-i\infty}^{c+i\infty} ds\, \exp(s \log(s) + x s) 
   For numerical purposes it is more convenient to use the following
   equivalent form of the integral,
   p(x) = (1/\pi) \int_0^\infty dt \exp(-t \log(t) - x t) \sin(\pi t).")

(defmfun landau-pdf (x)
  "gsl_ran_landau_pdf" ((x :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for the Landau distribution using an approximation to the formula given
   in #'landau.")

;;; Examples and unit test
#|
(make-tests landau
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect (landau rng)))
  (landau-pdf 0.25d0))
|#

(LISP-UNIT:DEFINE-TEST LANDAU
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 3880.037426254597d0 -0.6953200314545297d0
	  -0.02354364646600932d0 21.329209630030316d0
	  -0.3062224704714883d0 1.2424186669362394d0
	  26.146168479649152d0 4.337217640968217d0
	  1.6799546281085946d0 4.2475719218268395d0
	  4.681506208977819d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (LANDAU RNG)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.17331968995860203d0)
   (MULTIPLE-VALUE-LIST (LANDAU-PDF 0.25d0))))

