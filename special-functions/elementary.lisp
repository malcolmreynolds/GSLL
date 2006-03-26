;********************************************************
; file:        elementary.lisp                           
; description: Elementary functions                      
; date:        Mon Mar 20 2006 - 21:43                   
; author:      Liam M. Healy                             
; modified:    Sat Mar 25 2006 - 22:11
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl multiply ((x :double) (y :double))
  "gsl_sf_multiply_e"
  :documentation
  "Multiplies @var{x} and @var{y} returning the product and
associated error."
  :return (sf-result))

(defun-gsl multiply-err ((x :double) (dx :double) (y :double)  (dy :double))
  "gsl_sf_multiply_err_e"
  :documentation
  "Multiplies @var{x} and @var{y} with associated absolute
errors @var{dx} and @var{dy}.  The product 
@c{$xy \pm xy \sqrt{(dx/x)^2 +(dy/y)^2}$} 
@math{xy +/- xy \sqrt((dx/x)^2 +(dy/y)^2)} 
is returned."
  :return (sf-result))
