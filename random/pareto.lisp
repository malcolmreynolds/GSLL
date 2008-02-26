;; Pareto distribution
;; Liam Healy, Sat Oct  8 2006 - 21:23
;; Time-stamp: <2008-02-17 13:23:00EST pareto.lisp>
;; $Id$

(in-package :gsl)

(defmfun pareto (generator a b)
  "gsl_ran_pareto"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Pareto distribution of order a.
   The distribution function is
   p(x) dx = (a/b) / (x/b)^{a+1} dx
   x >= b.")

(defmfun pareto-pdf (x a b)
  "gsl_ran_pareto_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a Pareto distribution with exponent a and scale b, using
   the formula given in #'pareto.")

(defmfun pareto-P (x a b)
  "gsl_cdf_pareto_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the Pareto distribution with exponent a and scale b.")

(defmfun pareto-Q (x a b)
  "gsl_cdf_pareto_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the Pareto distribution with exponent a and scale b.")

(defmfun pareto-Pinv (P a b)
  "gsl_cdf_pareto_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the Pareto distribution with exponent a and scale b.")

(defmfun pareto-Qinv (Q a b)
  "gsl_cdf_pareto_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the Pareto distribution with exponent a and scale b.")

;;; Examples and unit test
#|
(make-tests pareto
  (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (pareto rng 1.0d0 2.0d0)))
  (pareto-pdf 1.5d0 1.3d0 1.0d0)
  (pareto-P 3.5d0 1.3d0 2.0d0)
  (pareto-Q 3.5d0 1.3d0 2.0d0)
  (pareto-Pinv 0.5168849835182453d0 1.3d0 2.0d0)
  (pareto-Qinv 0.4831150164817547d0 1.3d0 2.0d0))
|#

(LISP-UNIT:DEFINE-TEST PARETO
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 2.0005166356083666d0 12.276726596218747d0
	  7.076694965937239d0 2.111484074469764d0
	  8.633470811095883d0 4.123935696449152d0
	  2.0888231161547828d0 2.6870692498025632d0
	  3.703404287965457d0 2.7028744394290123d0
	  2.631773566385122d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (PARETO RNG 1.0d0 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5116034405707658d0)
   (MULTIPLE-VALUE-LIST (PARETO-PDF 1.5d0 1.3d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5168849835182453d0)
   (MULTIPLE-VALUE-LIST (PARETO-P 3.5d0 1.3d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4831150164817547d0)
   (MULTIPLE-VALUE-LIST (PARETO-Q 3.5d0 1.3d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 3.5000000000000004d0)
   (MULTIPLE-VALUE-LIST
    (PARETO-PINV 0.5168849835182453d0 1.3d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 3.5000000000000004d0)
   (MULTIPLE-VALUE-LIST
    (PARETO-QINV 0.4831150164817547d0 1.3d0 2.0d0))))

