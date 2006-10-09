;********************************************************
; file:        fdist.lisp                          
; description: Fdist distribution                  
; date:        Sat Sep 30 2006
; author:      Liam M. Healy                             
; modified:    Sun Oct  8 2006 - 16:31
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl fdist (generator nu1 nu2)
  "gsl_ran_fdist"
  (((generator generator) :pointer) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation
  "A random variate from the F-distribution with degrees of freedom @var{nu1}
   and @var{nu2}. The distribution function is
   p(x) dx = 
   { \Gamma((\nu_1 + \nu_2)/2)
        \over \Gamma(\nu_1/2) \Gamma(\nu_2/2) } 
   \nu_1^{\nu_1/2} \nu_2^{\nu_2/2} 
   x^{\nu_1/2 - 1} (\nu_2 + \nu_1 x)^{-\nu_1/2 -\nu_2/2}
   @math{x >= 0}.")

(defun-gsl fdist-pdf (x nu1 nu2)
  "gsl_ran_fdist_pdf" ((x :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for an F-distribution with @var{nu1} and @var{nu2} degrees of freedom,
   using the formula given #'fdist.")

(defun-gsl fdist-P (x nu1 nu2)
  "gsl_cdf_fdist_P" ((x :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for the fdist distribution with
  @var{nu1} and @var{nu2} degrees of freedom.")

(defun-gsl fdist-Q (x nu1 nu2)
  "gsl_cdf_fdist_Q" ((x :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for the fdist distribution with
  @var{nu1} and @var{nu2} degrees of freedom.")

(defun-gsl fdist-Pinv (P nu1 nu2)
  "gsl_cdf_fdist_Pinv" ((P :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for the fdist distribution with
  @var{nu1} and @var{nu2} degrees of freedom.")

(defun-gsl fdist-Qinv (Q nu1 nu2)
  "gsl_cdf_fdist_Qinv" ((Q :double) (nu1 :double) (nu2 :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
   @math{Q(x)} for the fdist distribution with
   @var{nu1} and @var{nu2} degrees of freedom.")

;;; Examples and unit test
(lisp-unit:define-test fdist
  (lisp-unit:assert-equal
   '("0.103774233365d+03" "0.212485013221d+01" "0.304410694709d+00"
     "0.300188687388d+00" "0.112282068448d-02" "0.292109400785d+00"
     "0.635729092565d-01" "0.477966365217d+00" "0.347211676079d-01"
     "0.486974823041d+00" "0.253179451696d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (fdist *rng-mt19937* 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.159471988462d+00"
   (fdist-pdf 1.2d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.612372435696d+00"
   (fdist-P 1.2d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.387627564304d+00"
   (fdist-Q 1.2d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000d+01"
   (fdist-Pinv 0.612372435695795d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000d+01"
   (fdist-Qinv 0.38762756430420503d0 1.0d0 2.0d0)))
