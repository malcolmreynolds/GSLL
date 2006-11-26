;********************************************************
; file:        poisson.lisp                          
; description: Poisson distribution                  
; date:        Sat Nov 25 2006 - 16:00
; author:      Liam M. Healy                             
; modified:    Sat Nov 25 2006 - 17:00
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl poisson (generator mu)
  "gsl_ran_poisson"
  (((generator generator) :pointer) (mu :double))
  :c-return :uint
  :documentation
  "A random integer from the Poisson distribution with mean @var{mu}.
   The probability distribution for Poisson variates is
   p(k) = {\mu^k \over k!} \exp(-\mu)
   @math{k >= 0}.")

(defun-gsl poisson-pdf (k mu)
  "gsl_ran_poisson_pdf" ((k :uint) (mu :double))
  :c-return :double
  :documentation
  "The probability @math{p(k)} of obtaining @var{k}
   from a Poisson distribution with mean @var{mu} using the formula
   given in #'poisson.")

(defun-gsl poisson-P (k mu)
  "gsl_cdf_poisson_P" ((k :uint) (mu :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(k)} for the Poisson distribution with parameter @var{mu}.")

(defun-gsl poisson-Q (k mu)
  "gsl_cdf_poisson_Q" ((k :uint) (mu :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(k)} for the Poisson distribution with parameter @var{mu}.")

;;; Examples and unit test
(lisp-unit:define-test poisson
  (lisp-unit:assert-equal
   '(15 6 9 9 5 8 11 9 11 5 10)
   (progn
     (rng-set *rng-mt19937* 0)
     (loop for i from 0 to 10
	collect
	(poisson *rng-mt19937* 10.0d0))))
  (lisp-unit:assert-first-fp-equal
   "0.112599032149d+00"
   (poisson-pdf 8 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.332819678751d+00"
   (poisson-P 8 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.667180321249d+00"
   (poisson-Q 8 10.0d0)))
