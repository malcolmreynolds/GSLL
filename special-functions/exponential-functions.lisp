;********************************************************
; file:        exponential-functions.lisp                
; description: Exponential functions                     
; date:        Tue Mar 21 2006 - 17:05                   
; author:      Liam M. Healy                             
; modified:    Tue Mar 21 2006 - 17:30
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Exponential Functions
;;;;****************************************************************************

(defun-sf gsl-exp ((x :double))
  "gsl_sf_exp_e"
  :documentation
  "The exponential function."
  :return (sf-result))

(defun-sf exp-scaled ((x :double))
  "gsl_sf_exp_e10_e"
  :documentation
  "The exponential function scaled. This function may be useful if the value
of @math{\exp(x)} would overflow the  numeric range of @code{double}."
  :return (sf-result-e10))

(defun-sf exp-mult ((x :double) (y :double))
  "gsl_sf_exp_mult_e"
  :documentation
  "Exponentiate @var{x} and multiply by the factor @var{y} to return the product @math{y \exp(x)}."
  :return (sf-result))

(defun-sf exp-mult-scaled ((x :double) (y :double))
  "gsl_sf_exp_mult_e10_e"
  :documentation
  "The product @math{y \exp(x)} with extended numeric range."
  :return (sf-result-e10))

;;;;****************************************************************************
;;;; Relative Exponential Functions
;;;;****************************************************************************

(defun-sf expm1 ((x :double))
  "gsl_sf_expm1_e"
  :documentation
  "@math{\exp(x)-1} using an algorithm that is accurate for small @math{x}."
  :return (sf-result))

(defun-sf exprel ((x :double))
  "gsl_sf_exprel_e"
  :documentation
  "@math{(\exp(x)-1)/x} using an algorithm that is accurate for small @math{x}.  For small @math{x} the algorithm is based on the expansion @math{(\exp(x)-1)/x = 1 + x/2 + x^2/(2*3) + x^3/(2*3*4) + \dots}."
  :return (sf-result))

(defun-sf exprel-2 ((x :double))
  "gsl_sf_exprel_2_e"
  :documentation
  "@math{2(\exp(x)-1-x)/x^2} using an algorithm that is accurate for small @math{x}.  For small @math{x} the algorithm is based on the expansion @math{2(\exp(x)-1-x)/x^2 = 1 + x/3 + x^2/(3*4) + x^3/(3*4*5) + \dots}."
  :return (sf-result))

(defun-sf exprel-n ((x :double))
  "gsl_sf_exprel_n_e"
  :documentation
  "@math{N}-relative exponential, which is the @var{n}-th generalization of the functions @code{gsl_sf_exprel} and @code{gsl_sf_exprel2}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Exponentiation With Error Estimate
;;;;****************************************************************************

(defun-sf exp-err ((x :double) (dx :double))
  "gsl_sf_exp_err_e"
  :documentation
  "Exponentiate @var{x} with an associated absolute error @var{dx}."
  :return (sf-result))

(defun-sf exp-err-scaled ((x :double) (dx :double))
  "gsl_sf_exp_err_e10_e"
  :documentation
  "Exponentiate @var{x} with an associated absolute error @var{dx} and with extended numeric range."
  :return (sf-result))

(defun-sf exp-mult-err ((x :double) (dx :double) (y :double) (dy :double))
  "gsl_sf_exp_mult_err_e"
  :documentation
  "The product @math{y \exp(x)} for the quantities @var{x}, @var{y} with associated absolute errors @var{dx}, @var{dy}."
  :return (sf-result))

(defun-sf exp-mult-err-scaled ((x :double) (y :double))
  "gsl_sf_exp_mult_err_e10_e"
  :documentation
  "The product @math{y \exp(x)} for the quantities @var{x}, @var{y} with associated absolute errors @var{dx}, @var{dy} and with extended numeric range."
  :return (sf-result-e10))
