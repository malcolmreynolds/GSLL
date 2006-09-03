;********************************************************
; file:        exponential.lisp                          
; description: Exponential distribution                  
; date:        Sat Sep  2 2006 - 19:04                   
; author:      Liam M. Healy                             
; modified:    Sat Sep  2 2006 - 19:17
;********************************************************
;;; $Id: $

(defun-gsl exponential (generator mu)
  "gsl_ran_exponential"
  (((generator generator) :pointer) (mu :double))
  :c-return :double
  :documentation
  "A random variate from the exponential distribution
   with mean @var{mu}. The distribution is
   p(x) dx = {1 \over \mu} \exp(-x/\mu) dx
   @math{x >= 0}.")

(defun-gsl exponential-pdf (x mu)
  "gsl_ran_exponential_pdf" ((x :double) (mu :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
  for an exponential distribution with mean @var{mu}, using the formula
  given for exponential.")

(defun-gsl exponential-P (x mu)
  "gsl_cdf_exponential_P" ((x :double) (mu :double))
  :c-return :double
  :documentation "The cumulative distribution function
   @math{P(x)} for the exponential distribution with mean @var{mu}.")

(defun-gsl exponential-Q (x mu)
  "gsl_cdf_exponential_Q" ((x :double) (mu :double))
  :c-return :double
  :documentation "The cumulative distribution function
   @math{Q(x)} for the exponential distribution with mean @var{mu}.")

(defun-gsl exponential-Pinv (P mu)
  "gsl_cdf_exponential_Pinv" ((P :double) (mu :double))
  :c-return :double
  :documentation "The inverse cumulative distribution function
   @math{P(x)} for the exponential distribution with mean @var{mu}.")

(defun-gsl exponential-Qinv (Q mu)
  "gsl_cdf_exponential_Qinv" ((Q :double) (mu :double))
  :c-return :double
  :documentation "The inverse cumulative distribution function
   @math{Q(x)} for the exponential distribution with mean @var{mu}.")

;;; Examples and unit test
(lisp-unit:define-test exponential
  (lisp-unit:assert-equal
   '("0.258284445884d-02" "0.181455814280d+02" "0.126365980543d+02"
     "0.542438725206d+00" "0.146249942342d+02" "0.723660792954d+01"
     "0.434536244968d+00" "0.295303920905d+01" "0.616105293907d+01"
     "0.301168633354d+01" "0.274510798194d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (exponential *rng-mt19937* 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+00"
   (exponential-pdf 0.0d0 10.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.393469340287d+00"
   (exponential-p 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.606530659713d+00"
   (exponential-q 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+01"
   (exponential-pinv 0.3934693402873666d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+01"
   (exponential-qinv 0.6065306597126334d0 2.0d0)))
