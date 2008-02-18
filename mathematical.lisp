;; Mathematical functions
;; Liam Healy, Wed Mar  8 2006 - 22:09
;; Time-stamp: <2008-02-17 09:13:19EST mathematical.lisp>
;; $Id: $

(in-package :gsl)

;;; Disable floating point traps for each CL implementation. 

;;; Not ported because they are all C macros or inline functions:
;;;   Mathematical Constants
;;;   Testing the Sign of Numbers
;;;   Testing for Odd and Even Numbers
;;;   Maximum and Minimum functions
;;; Does CL need the small integer powers?

;;;;****************************************************************************
;;; Infinities and Not-a-number
;;;;****************************************************************************

(defmacro pmnil (x)
  "+1, -1, or nil"
  `(let ((v ,x))
     (when (or (= 1 v) (= -1 v))
       v)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil)
  (import
   '(sb-ext:double-float-negative-infinity
     sb-ext:double-float-positive-infinity)))

(export '(+nan+ +positive-infinity+ +negative-infinity+))

(defconstant +nan+
  (ignore-errors
    (cffi:foreign-funcall "gsl_nan" :double)))

(defconstant +positive-infinity+
  (ignore-errors
    (cffi:foreign-funcall "gsl_posinf" :double)))

(defconstant +negative-infinity+
  (ignore-errors
    (cffi:foreign-funcall "gsl_neginf" :double)))

(defmfun nanp (x)
  "gsl_isnan" ((x :double))
  :c-return (cr :int)
  :return ((= 1 cr))
  :documentation			; FDL
  "Return T if x is a double-float NaN.")

(defmfun infinityp (x)
  "gsl_isinf" ((x :double))
  :c-return (cr :int)
  :return ((pmnil cr))
  :documentation			; FDL
  "Return +1 if x is positive infinity, -1 if negative infinity
   nil if finite.")

(defmfun finitep (x)
  "gsl_finite" ((x :double))
  :c-return (cr :int)
  :return ((= 1 cr))
  :documentation			; FDL
  "Return T if x is finite.")

;;;;****************************************************************************
;;; Elementary functions
;;;;****************************************************************************

(defmfun log+1 (x)
  "gsl_log1p" ((x :double))
  :c-return :double
  :documentation			; FDL
  "log(1+x), computed in a way that is accurate for small x.")

(defmfun exp-1 (x)
  "gsl_expm1" ((x :double))
  :c-return :double
  :documentation			; FDL
  "exp(x)-1, computed in a way that is accurate for small x.")

(defmfun hypotenuse* (x y)
  ;; This is redundant; there is "gsl_sf_hypot_e" defined as
  ;; #'hyptoenuse.
  "gsl_hypot" ((x :double) (y :double))
  :c-return :double
  :documentation			;FDL
  "The hypotenuse sqrt{x^2 + y^2} computed in a way that avoids overflow.")

;; Not clear why this function exists
(defmfun gsl-asinh (x)
   "gsl_asinh" ((x :double))
  :c-return :double
  :documentation			; FDL
  "Arc hyperbolic sine.")

;; Not clear why this function exists
(defmfun gsl-atanh (x)
   "gsl_atanh" ((x :double))
  :c-return :double
  :documentation			; FDL
  "Arc hyperbolic tangent.")

;;; gsl_ldexp
;;; gsl_frexp
;;; not mapped because CL has equivalents.

;;;;****************************************************************************
;;; Small integer powers
;;;;****************************************************************************

;;; Does CL need these?

#|
;; FDL
A common complaint about the standard C library is its lack of a
function for calculating (small) integer powers. GSL provides a
simple functions to fill this gap. For reasons of efficiency,
these functions do not check for overflow or underflow
conditions.

Function: double gsl_pow_int (double x, int n)
    This routine computes the power x^n for integer n. The power is computed efficiently--for example, x^8 is computed as ((x^2)^2)^2, requiring only 3 multiplications. A version of this function which also computes the numerical error in the result is available as gsl_sf_pow_int_e. 

Function: double gsl_pow_2 (const double x)
Function: double gsl_pow_3 (const double x)
Function: double gsl_pow_4 (const double x)
Function: double gsl_pow_5 (const double x)
Function: double gsl_pow_6 (const double x)
Function: double gsl_pow_7 (const double x)
Function: double gsl_pow_8 (const double x)
Function: double gsl_pow_9 (const double x)
    These functions can be used to compute small integer powers x^2, x^3, etc. efficiently. The functions will be inlined when possible so that use of these functions should be as efficient as explicitly writing the corresponding product expression.

|#

;;;; Testing the Sign of Numbers
;;; is all macros

;;;; Testing for Odd and Even Numbers
;;; is all macros

;;;; Maximum and Minimum functions
;;; is all macros and inline functions that have CL equivalents

;;;;****************************************************************************
;;; Approximate Comparison of Floating Point Numbers
;;;;****************************************************************************

;;; FDL
;;; It is sometimes useful to be able to compare two floating point
;;; numbers approximately, to allow for rounding and truncation
;;; errors. This function implements the approximate
;;; floating-point comparison algorithm proposed by D.E. Knuth in
;;; Section 4.2.2 of Seminumerical Algorithms (3rd edition).

(defmfun double-float-unequal (x y epsilon)
  "gsl_fcmp" ((x :double) (y :double) (epsilon :double))
  :c-return (cr :int)
  :documentation			; FDL
  "This function determines whether x and y are approximately equal
    to a relative accuracy epsilon.

    The relative accuracy is measured using an interval of size 2
    delta, where delta = 2^k \epsilon and k is the maximum
    base-2 exponent of x and y as computed by the function
    frexp().

    If x and y lie within this interval, they are considered
    approximately equal and the function returns nil. Otherwise if
    x < y, the function returns -1, or if x > y, the function
    returns +1."
  :return ((pmnil cr)))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests mathematical
  (log+1 0.001d0)
  (exp-1 0.001d0)
  (hypotenuse 3.0d0 4.0d0))
|#

(lisp-unit:define-test mathematical
  (lisp-unit:assert-true (nanp +nan+))
  (lisp-unit:assert-false (nanp 1.0d0))
  (lisp-unit:assert-true (finitep 1.0d0))
  (lisp-unit:assert-false (infinityp 1.0d0))
  (lisp-unit:assert-eq 1 (infinityp +positive-infinity+))
  (lisp-unit:assert-false (finitep +positive-infinity+))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 9.995003330835331d-4)
   (MULTIPLE-VALUE-LIST (LOG+1 0.001d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.0010005001667083417d0)
   (MULTIPLE-VALUE-LIST (EXP-1 0.001d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 5.0d0 2.220446049250313d-15)
   (MULTIPLE-VALUE-LIST (HYPOTENUSE 3.0d0 4.0d0)))
  (lisp-unit:assert-false (double-float-unequal 1.0d0 1.0d0 0.001d0)))
