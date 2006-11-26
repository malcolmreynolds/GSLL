;********************************************************
; file:        geometric.lisp                          
; description: Geometric distribution
; date:        Sat Nov 25 2006 - 16:00
; author:      Liam M. Healy                             
; modified:    Sat Nov 25 2006 - 21:56
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl geometric (generator p)
  "gsl_ran_geometric"
  (((generator generator) :pointer) (p :double))
  :c-return :uint
  :documentation
  "A random integer from the geometric distribution,
   the number of independent trials with probability @var{p} until the
   first success.  The probability distribution for geometric variates
   is p(k) =  p (1-p)^{k-1} for @math{k >= 1}.
   Note that the distribution begins with @math{k=1} with this
   definition.  There is another convention in which the exponent @math{k-1} 
   is replaced by @math{k}.")

(defun-gsl geometric-pdf (k p)
  "gsl_ran_geometric_pdf" ((k :uint) (p :double))
  :c-return :double
  :documentation
  "The probability @math{p(k)} of obtaining @var{k}
   from a geometric distribution with probability parameter @var{p}, using
   the formula given in #'geometric.")

(defun-gsl geometric-P (k p)
  "gsl_cdf_geometric_P" ((k :uint) (p :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(k)} for the Geometric distribution with parameter @var{p}.")

(defun-gsl geometric-Q (k p)
  "gsl_cdf_geometric_Q" ((k :uint) (p :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(k)} for the Geometric distribution with parameters @var{p}.")

;;; Examples and unit test
(lisp-unit:define-test geometric
  (lisp-unit:assert-equal
   '(1 4 3 1 3 2 1 1 2 1 1)
   (progn
     (rng-set *rng-mt19937* 0)
     (loop for i from 0 to 10
	collect
	(geometric *rng-mt19937* 0.4d0))))
  (lisp-unit:assert-first-fp-equal
   "0.240000000000d+00"
   (geometric-pdf 2 0.4d0))
  (lisp-unit:assert-first-fp-equal
   "0.640000000000d+00"
   (geometric-P 2 0.4d0))
  (lisp-unit:assert-first-fp-equal
   "0.360000000000d+00"
   (geometric-Q 2 0.4d0)))
