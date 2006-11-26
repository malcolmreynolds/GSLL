;********************************************************
; file:        bernoulli.lisp                          
; description: Bernoulli distribution                  
; date:        Sat Nov 25 2006 - 16:59
; author:      Liam M. Healy                             
; modified:    Sat Nov 25 2006 - 17:01
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl bernoulli (generator p)
  "gsl_ran_bernoulli"
  (((generator generator) :pointer) (p :double))
  :c-return :uint
  :documentation
  "Returns either 0 or 1, the result of a Bernoulli trial
   with probability @var{p}.  The probability distribution for
   a Bernoulli trial is
   p(0) = 1 - p
   p(1) = p.")

(defun-gsl bernoulli-pdf (k p)
  "gsl_ran_bernoulli_pdf" ((k :uint) (p :double))
  :c-return :double
  :documentation "The probability @math{p(k)} of obtaining
  @var{k} from a Bernoulli distribution with probability parameter
  @var{p}, using the formula given in #'bernoulli.")

;;; Examples and unit test
(lisp-unit:define-test bernoulli
  (lisp-unit:assert-equal
   '(0 1 1 0 1 1 0 0 0 0 0)
   (progn
     (rng-set *rng-mt19937* 0)
     (loop for i from 0 to 10
	collect
	(bernoulli *rng-mt19937* 0.5d0))))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d+00"
   (bernoulli-pdf 0 0.5d0)))
