;; Negative binomial and Pascal distributions
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-10-25 18:06:14EDT negative-binomial.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Negative binomial
;;;;****************************************************************************

(defmfun negative-binomial (generator p n)
  "gsl_ran_negative_binomial"
  (((generator generator) :pointer) (p :double) (n :double))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the negative binomial
   distribution, the number of failures occurring before n successes
   in independent trials with probability p of success.  The
   probability distribution for negative binomial variates is,
   p(k) = {\Gamma(n + k) \over \Gamma(k+1) \Gamma(n) } p^n (1-p)^k
   Note that n is not required to be an integer.")

(defmfun negative-binomial-pdf (k p n)
  "gsl_ran_negative_binomial_pdf" ((k :uint) (p :double) (n :double))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a negative binomial distribution with parameters p and
   n, using the formula given in #'negative-binomial.")

(defmfun negative-binomial-P (k p n)
  "gsl_cdf_negative_binomial_P" ((k :uint) (p :double) (n :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the negative binomial distribution
  with parameters p and n.")

(defmfun negative-binomial-Q (k p n)
  "gsl_cdf_negative_binomial_Q" ((k :uint) (p :double) (n :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(k) for the negative binomial distribution
   with parameters p and n.")

;;;;****************************************************************************
;;;; Pascal
;;;;****************************************************************************

(defmfun pascal (generator p n)
  "gsl_ran_pascal"
  (((generator generator) :pointer) (p :double) (n :uint))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the Pascal distribution.  The
   Pascal distribution is simply a negative binomial distribution with an
   integer value of n.
   p(k) = {(n + k - 1)! \over k! (n - 1)! } p^n (1-p)^k
   k >= 0.")

(defmfun pascal-pdf (k p n)
  "gsl_ran_pascal_pdf" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a Pascal distribution with parameters p and
   n, using the formula given in #'pascal.")

(defmfun pascal-P (k p n)
  "gsl_cdf_pascal_P" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the Pascal distribution
  with parameters p and n.")

(defmfun pascal-Q (k p n)
  "gsl_cdf_pascal_Q" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(k) for the Pascal distribution
   with parameters p and n.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(save-test negative-binomial
  (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (negative-binomial rng 0.4d0 12.0d0)))
  (negative-binomial-pdf 5 0.4d0 12.0d0)
  (negative-binomial-P 5 0.4d0 12.0d0)
  (negative-binomial-Q 5 0.4d0 12.0d0)
  (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (pascal rng 0.4d0 12)))
  (pascal-pdf 5 0.4d0 12)
  (pascal-P 5 0.4d0 12)
  (pascal-Q 5 0.4d0 12))

(LISP-UNIT:DEFINE-TEST NEGATIVE-BINOMIAL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 15 21 19 15 8 18 23 18 33 16 10))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (NEGATIVE-BINOMIAL RNG 0.4 12.0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0056984767089869)
   (MULTIPLE-VALUE-LIST
    (NEGATIVE-BINOMIAL-PDF 5 0.4 12.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.010594202555514881)
   (MULTIPLE-VALUE-LIST (NEGATIVE-BINOMIAL-P 5 0.4 12.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9894057974444851)
   (MULTIPLE-VALUE-LIST (NEGATIVE-BINOMIAL-Q 5 0.4 12.0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 15 21 19 15 8 18 23 18 33 16 10))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (PASCAL RNG 0.4 12)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0056984767089869)
   (MULTIPLE-VALUE-LIST (PASCAL-PDF 5 0.4 12)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.010594202555514881)
   (MULTIPLE-VALUE-LIST (PASCAL-P 5 0.4 12)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9894057974444851)
   (MULTIPLE-VALUE-LIST (PASCAL-Q 5 0.4 12))))


