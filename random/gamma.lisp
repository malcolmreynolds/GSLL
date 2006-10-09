;********************************************************
; file:        gamma.lisp                          
; description: Gamma distribution                  
; date:        Sat Sep 30 2006
; author:      Liam M. Healy                             
; modified:    Sat Oct  7 2006 - 11:53
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl gamma-rd (generator a b)
  ;; Named #'gamma-rd to avoid confusion with the special function #'gamma.
  "gsl_ran_gamma"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation
  "A random variate from the gamma distribution.
   The distribution function is
   p(x) dx = {1 \over \Gamma(a) b^a} x^{a-1} e^{-x/b} dx
   for @math{x > 0}. The gamma distribution with an integer parameter @var{a}
   is known as the Erlang distribution.  The variates are computed using
   the algorithms from Knuth (vol 2).")

(defun-gsl gamma-mt (generator a b)
  "gsl_ran_gamma_mt"
  (((generator generator) :pointer) (a :double) (b :double))
  :c-return :double
  :documentation
  "A gamma variate using the Marsaglia-Tsang fast gamma method.")

(defun-gsl gamma-pdf (x a b)
  "gsl_ran_gamma_pdf" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation
  "The probability density @math{p(x)} at @var{x}
   for a gamma distribution with parameters @var{a} and @var{b}, using the
   formula given in #'gamma.")

(defun-gsl gamma-P (x a b)
  "gsl_cdf_gamma_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{P(x)} for the Gamma distribution with parameters @var{a} and @var{b}.")

(defun-gsl gamma-Q (x a b)
  "gsl_cdf_gamma_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation "The cumulative distribution functions
  @math{Q(x)} for the Gamma distribution with parameters @var{a} and @var{b}.")

(defun-gsl gamma-Pinv (P a b)
  "gsl_cdf_gamma_Pinv" ((P :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
  @math{P(x)} for the Gamma distribution with parameters @var{a} and @var{b}.")

(defun-gsl gamma-Qinv (Q a b)
  "gsl_cdf_gamma_Qinv" ((Q :double) (a :double) (b :double))
  :c-return :double
  :documentation  "The inverse cumulative distribution functions
   @math{Q(x)} for the Gamma distribution with parameters @var{a} and @var{b}.")

;;; Examples and unit test
(lisp-unit:define-test gamma-randist
  (lisp-unit:assert-equal
   '("0.516568891768d-03" "0.362911628560d+01" "0.252731961087d+01"
     "0.108487745041d+00" "0.292499884683d+01" "0.144732158591d+01"
     "0.869072489937d-01" "0.590607841809d+00" "0.123221058781d+01"
     "0.602337266708d+00" "0.549021596387d+00")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (gamma-rd *rng-mt19937* 1.0d0 2.0d0)))))
  (lisp-unit:assert-equal
   '("0.260013787613d+01" "0.252226669542d+01" "0.773142209213d+01"
     "0.422727992649d+01" "0.951930434749d-01" "0.571092010687d+00"
     "0.891063771946d+00" "0.826322120255d+00" "0.318306657206d+01"
     "0.380840036132d-02" "0.103201173341d+01")
   (lisp-unit:fp-sequence
    (progn
      (rng-set *rng-mt19937* 0)
      (loop for i from 0 to 10
	    collect
	    (gamma-mt *rng-mt19937* 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.475614712250d+00"
   (gamma-pdf 0.1d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.487705754993d-01"
   (gamma-P 0.1d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.951229424501d+00"
   (gamma-Q 0.1d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+00"
   (gamma-Pinv 0.048770575499286005d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.100000000000d+00"
   (gamma-Qinv 0.951229424500714d0 1.0d0 2.0d0)))
