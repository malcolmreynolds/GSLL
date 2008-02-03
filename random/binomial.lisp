;; Binomial distribution
;; Liam Healy, Sat Nov 25 2006 - 16:00
;; Time-stamp: <2008-02-03 11:25:08EST binomial.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl binomial (generator p n)
  "gsl_ran_binomial"
  (((generator generator) :pointer) (p :double) (n :uint))
  :c-return :uint
  :documentation			; FDL
  "A random integer from the binomial distribution,
  the number of successes in n independent trials with probability
  p.  The probability distribution for binomial variates is,
  p(k) = {n! \over k! (n-k)!} p^k (1-p)^{n-k}
  0 <= k <= n.")

(defun-gsl binomial-pdf (k p n)
  "gsl_ran_binomial_pdf" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The probability p(k) of obtaining k
   from a binomial distribution with parameters p and n, using
   the formula given in #'binomial.")

(defun-gsl binomial-P (k p n)
  "gsl_cdf_binomial_P" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(k) for the Binomial distribution with parameters p and n.")

(defun-gsl binomial-Q (k p n)
  "gsl_cdf_binomial_Q" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(k) for the Binomial distribution
   with parameters p and n.")

;;; Examples and unit test
(lisp-unit:define-test binomial
  (lisp-unit:assert-equal
   '(11 3 4 8 4 5 8 6 5 6 6)
   (letm ((rng (random-number-generator *mt19937* 0)))
     (loop for i from 0 to 10
	   collect
	   (binomial rng 0.4d0 12))))
  (lisp-unit:assert-first-fp-equal
   "0.227030335488d+00"
   (binomial-pdf 5 0.4d0 12))
  (lisp-unit:assert-first-fp-equal
   "0.665208557568d+00"
   (binomial-P 5 0.4d0 12))
  (lisp-unit:assert-first-fp-equal
   "0.334791442432d+00"
   (binomial-Q 5 0.4d0 12)))
