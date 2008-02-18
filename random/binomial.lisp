;; Binomial distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-02-17 13:46:01EST binomial.lisp>
;; $Id: $

(in-package :gsl)

(defmfun binomial (generator p n)
  "gsl_ran_binomial"
  (((generator generator) :pointer) (p :double) (n :uint))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the binomial distribution,
  the number of successes in n independent trials with probability
  p.  The probability distribution for binomial variates is,
  p(k) = {n! \over k! (n-k)!} p^k (1-p)^{n-k}
  0 <= k <= n.")

(defmfun binomial-pdf (k p n)
  "gsl_ran_binomial_pdf" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a binomial distribution with parameters p and n, using
   the formula given in #'binomial.")

(defmfun binomial-P (k p n)
  "gsl_cdf_binomial_P" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the Binomial distribution with parameters p and n.")

(defmfun binomial-Q (k p n)
  "gsl_cdf_binomial_Q" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(k) for the Binomial distribution
   with parameters p and n.")

;;; Examples and unit test
#|
(make-tests binomial
  (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (binomial rng 0.4d0 12)))
  (binomial-pdf 5 0.4d0 12)
  (binomial-P 5 0.4d0 12)
  (binomial-Q 5 0.4d0 12))
|#

(LISP-UNIT:DEFINE-TEST BINOMIAL
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST 11 3 4 8 4 5 8 6 5 6 6))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (BINOMIAL RNG 0.4d0 12)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.22703033548799986d0)
   (MULTIPLE-VALUE-LIST (BINOMIAL-PDF 5 0.4d0 12)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6652085575680018d0)
   (MULTIPLE-VALUE-LIST (BINOMIAL-P 5 0.4d0 12)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.33479144243199815d0)
   (MULTIPLE-VALUE-LIST (BINOMIAL-Q 5 0.4d0 12))))

