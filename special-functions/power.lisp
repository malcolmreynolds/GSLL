;********************************************************
; file:        power.lisp                                
; description: Power                                     
; date:        Sun Apr 30 2006 - 22:46                   
; author:      Liam M. Healy                             
; modified:    Sun Apr 30 2006 - 22:48
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl pow ((x :double) (n :int))
  "gsl_sf_pow_int_e"
  :documentation "The power @math{x^n} for integer @var{n}.  The
  power is computed using the minimum number of multiplications. For
  example, @math{x^8} is computed as @math{((x^2)^2)^2}, requiring only 3
  multiplications.  For reasons of efficiency, these functions do not
  check for overflow or underflow conditions."
  :return (sf-result))

