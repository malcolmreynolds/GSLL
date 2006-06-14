;********************************************************
; file:        exponential-functions.lisp                
; description: Exponential functions                     
; date:        Tue Mar 21 2006 - 17:05                   
; author:      Liam M. Healy                             
; modified:    Tue Jun 13 2006 - 21:36
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Exponential Functions
;;;;****************************************************************************

(defun-gsl gsl-exp (x)
  "gsl_sf_exp_e" ((x :double) (ret sf-result))
  :documentation "The exponential function.")

(defun-gsl exp-scaled (x)
  "gsl_sf_exp_e10_e" ((x :double) (ret sf-result-e10))
  :documentation
  "The exponential function scaled. This function may be useful if the value
   of @math{\exp(x)} would overflow the  numeric range of @code{double}.")

(defun-gsl exp-mult (x y)
  "gsl_sf_exp_mult_e" ((x :double) (y :double) (ret sf-result))
  :documentation "Exponentiate @var{x} and multiply by the
  factor @var{y} to return the product @math{y \exp(x)}.")

(defun-gsl exp-mult-scaled (x y)
  "gsl_sf_exp_mult_e10_e" ((x :double) (y :double) (ret sf-result-e10))
  :documentation
  "The product @math{y \exp(x)} with extended numeric range.")

;;;;****************************************************************************
;;;; Relative Exponential Functions
;;;;****************************************************************************

(defun-gsl expm1 (x)
  "gsl_sf_expm1_e" ((x :double) (ret sf-result))
  :documentation
  "@math{\exp(x)-1} using an algorithm that is accurate for small @math{x}.")

(defun-gsl exprel (x)
  "gsl_sf_exprel_e" ((x :double) (ret sf-result))
  :documentation
  "@math{(\exp(x)-1)/x} using an algorithm that is accurate for small @math{x}.
  For small @math{x} the algorithm is based on the expansion
  @math{(\exp(x)-1)/x = 1 + x/2 + x^2/(2*3) + x^3/(2*3*4) + \dots}.")

(defun-gsl exprel-2 (x)
  "gsl_sf_exprel_2_e" ((x :double) (ret sf-result))
  :documentation
  "@math{2(\exp(x)-1-x)/x^2} using an algorithm that is accurate for small
   @math{x}.  For small @math{x} the algorithm is based on the expansion
   @math{2(\exp(x)-1-x)/x^2 = 1 + x/3 + x^2/(3*4) + x^3/(3*4*5) + \dots}.")

(defun-gsl exprel-n (n x)
  "gsl_sf_exprel_n_e" ((n :int) (x :double) (ret sf-result))
  :documentation
  "@math{N}-relative exponential, which is the @var{n}-th generalization
   of the functions @code{gsl_sf_exprel} and @code{gsl_sf_exprel2}.")

;;;;****************************************************************************
;;;; Exponentiation With Error Estimate
;;;;****************************************************************************

(defun-gsl exp-err (x dx)
  "gsl_sf_exp_err_e" ((x :double) (dx :double) (ret sf-result))
  :documentation
  "Exponentiate @var{x} with an associated absolute error @var{dx}.")

(defun-gsl exp-err-scaled (x dx)
  "gsl_sf_exp_err_e10_e"
  ((x :double) (dx :double) (ret sf-result))
  :documentation
  "Exponentiate @var{x} with an associated absolute error @var{dx}
  and with extended numeric range.")

(defun-gsl exp-mult-err (x dx y dy)
  "gsl_sf_exp_mult_err_e"
  ((x :double) (dx :double) (y :double) (dy :double) (ret sf-result))
  :documentation
  "The product @math{y \exp(x)} for the quantities @var{x},
   @var{y} with associated absolute errors @var{dx}, @var{dy}.")

(defun-gsl exp-mult-err-scaled (x y)
  "gsl_sf_exp_mult_err_e10_e" ((x :double) (y :double) (ret sf-result-e10))
  :documentation
  "The product @math{y \exp(x)} for the quantities @var{x}, @var{y}
   with associated absolute errors @var{dx}, @var{dy} and with
   extended numeric range.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test exponential-functions
  (lisp-unit:assert-first-fp-equal
   "0.200855369232d+02"
   (gsl-exp 3.0d0))
  (lisp-unit:assert-equal
   '("0.108003407162d+01" "0.241000000000e+03")
   (subseq (lisp-unit:fp-values (exp-scaled 555.0d0)) 0 2))
  (lisp-unit:assert-first-fp-equal
   "0.365352998968d+45"
   (exp-mult 101.0d0 5.0d0))
  (lisp-unit:assert-equal
   '("0.109083441234d+01" "0.243000000000e+03")
   (subseq (lisp-unit:fp-values (exp-mult-scaled 555.0d0 101.0d0)) 0 2))
  (lisp-unit:assert-first-fp-equal
   "0.100005000167d-03"
   (expm1 0.0001d0))
  (lisp-unit:assert-first-fp-equal
   "0.100005000167d+01"
   (exprel 0.0001d0))
  (lisp-unit:assert-first-fp-equal
   "0.100033341668d+01"
   (exprel-2 0.001d0))
  (lisp-unit:assert-first-fp-equal
   "0.100025005001d+01"
   (exprel-n 3 0.001d0))
  (lisp-unit:assert-first-fp-equal
   "0.200855369232d+02"
   (exp-err 3.0d0 0.001d0))
  (lisp-unit:assert-first-fp-equal
   "0.461967349233d+03"
   (exp-mult-err 3.0d0 0.001d0 23.0d0 0.001d0)))
