;********************************************************
; file:        power.lisp                                
; description: Power                                     
; date:        Sun Apr 30 2006 - 22:46                   
; author:      Liam M. Healy                             
; modified:    Sat Jun 17 2006 - 22:26
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl pow (x n)
  "gsl_sf_pow_int_e" ((x :double) (n :int) (ret sf-result))
  :documentation "The power @math{x^n} for integer @var{n}.  The
  power is computed using the minimum number of multiplications. For
  example, @math{x^8} is computed as @math{((x^2)^2)^2}, requiring only 3
  multiplications.  For reasons of efficiency, these functions do not
  check for overflow or underflow conditions.")

(lisp-unit:define-test power
  (lisp-unit:assert-first-fp-equal
   "0.525218750000d+03"
   (pow 3.5d0 5)))
