;; Logarithmic distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-02-17 15:36:14EST logarithmic.lisp>
;; $Id$

(in-package :gsl)

(defmfun logarithmic (generator p)
  "gsl_ran_logarithmic"
  (((generator generator) :pointer) (p :double))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the logarithmic distribution.
   The probability distribution for logarithmic random variates
   is p(k) = {-1 \over \log(1-p)} {\left( p^k \over k \right)}
   for k >= 1.")

(defmfun logarithmic-pdf (k p)
  "gsl_ran_logarithmic_pdf" ((k :uint) (p :double))
  :c-return :double
  :documentation
  "The probability p(k) of obtaining k
   from a logarithmic distribution with probability parameter p,
   using the formula given in #'logarithmic.")

;;; Examples and unit test
#|
(make-tests logarithmic
  (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (logarithmic rng 0.9d0)))
  (logarithmic-pdf 2 0.4d0))
|#

(LISP-UNIT:DEFINE-TEST LOGARITHMIC
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 1 3 1 4 1 1 2 1 1 5 2))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (LOGARITHMIC RNG 0.9d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.15660921511769743d0)
   (MULTIPLE-VALUE-LIST (LOGARITHMIC-PDF 2 0.4d0))))

