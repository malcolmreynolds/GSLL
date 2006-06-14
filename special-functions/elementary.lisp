;********************************************************
; file:        elementary.lisp                           
; description: Elementary functions                      
; date:        Mon Mar 20 2006 - 21:43                   
; author:      Liam M. Healy                             
; modified:    Tue Jun 13 2006 - 21:04
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl multiply (x y)
  "gsl_sf_multiply_e"
  ((x :double) (y :double) (ret sf-result))
  :documentation
  "Multiplies @var{x} and @var{y} returning the product and
  associated error.")

(defun-gsl multiply-err (x dx y dy)
    "gsl_sf_multiply_err_e"
  ((x :double) (dx :double) (y :double)  (dy :double) (ret sf-result))
  :documentation
  "Multiplies @var{x} and @var{y} with associated absolute
   errors @var{dx} and @var{dy}.  The product 
   @math{xy +/- xy \sqrt((dx/x)^2 +(dy/y)^2)} 
   is returned.")

(lisp-unit:define-test elementary
  (lisp-unit:assert-first-fp-equal
   "0.600000000000d+01"
   (multiply 3.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.600000000000d+01"
   (multiply-err 3.0d0 0.1d0 2.0d0 0.1d0)))
