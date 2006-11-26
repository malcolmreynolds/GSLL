;********************************************************
; file:        binomial.lisp                          
; description: Binomial distribution
; date:        Sat Nov 25 2006 - 16:00
; author:      Liam M. Healy                             
; modified:    Sat Nov 25 2006 - 18:08
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl binomial (generator p n)
  "gsl_ran_binomial"
  (((generator generator) :pointer) (p :double) (n :uint))
  :c-return :uint
  :documentation
  "A random integer from the binomial distribution,
  the number of successes in @var{n} independent trials with probability
  @var{p}.  The probability distribution for binomial variates is,
  p(k) = {n! \over k! (n-k)!} p^k (1-p)^{n-k}
  @math{0 <= k <= n}.")

(defun-gsl binomial-pdf (k p n)
  "gsl_ran_binomial_pdf" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation
  "The probability @math{p(k)} of obtaining @var{k}
   from a binomial distribution with parameters @var{p} and @var{n}, using
   the formula given in #'binomial.")

(defun-gsl binomial-P (k p n)
  "gsl_cdf_binomial_P" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(k)} for the Binomial distribution
  with parameters @var{p} and @var{n}.")

(defun-gsl binomial-Q (k p n)
  "gsl_cdf_binomial_Q" ((k :uint) (p :double) (n :uint))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(k)} for the Binomial distribution
   with parameters @var{p} and @var{n}.")

;;; Examples and unit test
(lisp-unit:define-test binomial
  (lisp-unit:assert-equal
   '(11 3 4 8 4 5 8 6 5 6 6)
   (progn
     (rng-set *rng-mt19937* 0)
     (loop for i from 0 to 10
	collect
	(binomial *rng-mt19937* 0.4d0 12))))
  (lisp-unit:assert-first-fp-equal
   "0.227030335488d+00"
   (binomial-pdf 5 0.4d0 12))
  (lisp-unit:assert-first-fp-equal
   "0.665208557568d+00"
   (binomial-P 5 0.4d0 12))
  (lisp-unit:assert-first-fp-equal
   "0.334791442432d+00"
   (binomial-Q 5 0.4d0 12)))
