;********************************************************
; file:        hypergeometric.lisp                          
; description: Hypergeometric distribution
; date:        Sat Nov 25 2006 - 16:00
; author:      Liam M. Healy                             
; modified:    Sat Nov 25 2006 - 22:40
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl hypergeometric (generator n1 n2 tt)
  "gsl_ran_hypergeometric"
  (((generator generator) :pointer) (n1 :uint) (n2 :uint)(tt :uint))
  :c-return :uint
  :documentation
  "A random integer from the hypergeometric
   distribution.  The probability distribution for hypergeometric
   random variates is
   p(k) =  C(n_1, k) C(n_2, t - k) / C(n_1 + n_2, t)
   where @math{C(a,b) = a!/(b!(a-b)!)} and 
   @math{t <= n_1 + n_2}.  The domain of @math{k} is 
   @math{max(0,t-n_2), ..., min(t,n_1)}.
   If a population contains @math{n_1} elements of ``type 1'' and
   @math{n_2} elements of ``type 2'' then the hypergeometric
   distribution gives the probability of obtaining @math{k} elements of
   ``type 1'' in @math{t} samples from the population without
   replacement.")

(defun-gsl hypergeometric-pdf (k n1 n2 tt)
  "gsl_ran_hypergeometric_pdf" ((k :uint) (n1 :uint) (n2 :uint)(tt :uint))
  :c-return :double
  :documentation
  "The probability @math{p(k)} of obtaining @var{k}
   from a hypergeometric distribution with parameters @var{n1}, @var{n2},
   @var{tt}, using the formula given in #'hypergeometric.")

(defun-gsl hypergeometric-P (k n1 n2 tt)
  "gsl_cdf_hypergeometric_P" ((k :uint) (n1 :uint) (n2 :uint)(tt :uint))
  :c-return :double
  :documentation
  "The cumulative distribution functions @math{P(k)} for the
   hypergeometric distribution with parameters @var{n1}, @var{n2}
   and @var{tt}.")

(defun-gsl hypergeometric-Q  (k n1 n2 tt)
  "gsl_cdf_hypergeometric_Q"  ((k :uint) (n1 :uint) (n2 :uint)(tt :uint))
  :c-return :double
  :documentation
  "The cumulative distribution functions @math{Q(k)} for the
   hypergeometric distribution with parameters @var{n1}, @var{n2}
   and @var{tt}.")

;;; Examples and unit test
(lisp-unit:define-test hypergeometric-randist
  (lisp-unit:assert-equal
   '(2 1 0 0 1 1 3 1 0 1 3)
   (progn
     (rng-set *rng-mt19937* 0)
     (loop for i from 0 to 10
	collect
	(hypergeometric *rng-mt19937* 3 6 3))))
  (lisp-unit:assert-first-fp-equal
   "0.357142857143d+00"
   (hypergeometric-pdf 0 2 6 3))
  (lisp-unit:assert-first-fp-equal
   "0.892857142857d+00"
   (hypergeometric-P 1 2 6 3))
  (lisp-unit:assert-first-fp-equal
   "0.107142857143d+00"
   (hypergeometric-Q 1 2 6 3)))
