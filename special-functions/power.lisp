;; Integer powers
;; Liam Healy, Sun Apr 30 2006 - 22:46
;; Time-stamp: <2008-02-16 22:39:17EST power.lisp>
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

#|
(make-tests power
  (pow 3.5d0 5))
|#

(LISP-UNIT:DEFINE-TEST POWER
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 525.21875d0 9.329759187437503d-13)
   (MULTIPLE-VALUE-LIST (POW 3.5d0 5))))
