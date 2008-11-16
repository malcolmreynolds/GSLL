;; Integer powers
;; Liam Healy, Sun Apr 30 2006 - 22:46
;; Time-stamp: <2008-11-15 22:32:30EST power.lisp>
;; $Id$

(in-package :gsl)

(defmfun pow (x n)
  "gsl_sf_pow_int_e" ((x :double) (n :int) (ret sf-result))
  :documentation			; FDL
  "The power x^n for integer n.  The
  power is computed using the minimum number of multiplications. For
  example, x^8 is computed as ((x^2)^2)^2, requiring only 3
  multiplications.  For reasons of efficiency, these functions do not
  check for overflow or underflow conditions.")

(save-test power
  (pow 3.5d0 5))
