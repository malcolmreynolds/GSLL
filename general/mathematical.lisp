;********************************************************
; file:        mathematical.lisp                         
; description: Mathematical functions                    
; date:        Wed Mar  8 2006 - 22:09                   
; author:      Liam M. Healy
; modified:    Thu Mar 23 2006 - 14:33
;********************************************************

(in-package :gsl)

(export '(+nan+ +positive-infinity+ +negative-infinity+
	  nanp infinityp finitep log+1 exp-1 hypotenuse approximately=))

;;;; Mathematical Constants
;;; Is all macros


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

(defconstant +nan+
  (ignore-errors
    (cffi:foreign-funcall "gsl_nan" :double)))

(defconstant +positive-infinity+
  (ignore-errors
    (cffi:foreign-funcall "gsl_posinf" :double)))

(defconstant +negative-infinity+
  (ignore-errors
    (cffi:foreign-funcall "gsl_neginf" :double)))

(defunx-map nanp "gsl_isnan" (x)
  "Return T if x is a double-float NaN."
  (= 1
     (cffi:foreign-funcall
      "gsl_isnan"
      :double x
      :int)))

(defunx-map infinityp "gsl_isinf" (x)
  "Return +1 if x is positive infinity, -1 if negative infinity
   nil if finite."
  (pmnil
   (cffi:foreign-funcall
    "gsl_isinf"
    :double x
    :int)))

(defunx-map finitep "gsl_finite" (x)
  "Return T if finite."
  (= 1
     (cffi:foreign-funcall
      "gsl_finite"
      :double x
      :int)))

;;;;****************************************************************************
;;; Elementary functions
;;;;****************************************************************************

(defunx-map log+1 "gsl_log1p" (x)
  "log(1+x), computed in a way that is accurate for small x."
  (cffi:foreign-funcall
   "gsl_log1p"
   :double x
   :double))

(defunx-map exp-1 "gsl_expm1" (x)
  "exp(x)-1, computed in a way that is accurate for small x."
  (cffi:foreign-funcall
   "gsl_expm1"
   :double x
   :double))

(defunx-map hypotenuse "gsl_hypot" (x y)
  "sqrt{x^2 + y^2} computed in a way that avoids overflow."
  (cffi:foreign-funcall
   "gsl_hypot"
   :double x
   :double y
   :double))

;; Not clear why this function exists
(defunx-map gsl-asinh "gsl_asinh" (x)
  "arcsinh"
  (cffi:foreign-funcall
   "gsl_asinh"
   :double x
   :double))

;;; Not clear why this function exists
(defunx-map gsl-atanh "gsl_atanh" (x)
  "arctanh"
  (cffi:foreign-funcall
   "gsl_atanh"
   :double x
   :double))

;;; gsl_ldexp
;;; gsl_frexp
;;; not mapped because CL has equivalents.

;;;;****************************************************************************
;;; Small integer powers
;;;;****************************************************************************

;;; Does CL need these?

#|
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

;;; It is sometimes useful to be able to compare two floating point
;;; numbers approximately, to allow for rounding and truncation
;;; errors. This function implements the approximate
;;; floating-point comparison algorithm proposed by D.E. Knuth in
;;; Section 4.2.2 of Seminumerical Algorithms (3rd edition).

(defunx-map double-float-equal "gsl_fcmp" (x y epsilon)
  "This function determines whether x and y are approximately equal
    to a relative accuracy epsilon.

    The relative accuracy is measured using an interval of size 2
    \delta, where \delta = 2^k \epsilon and k is the maximum
    base-2 exponent of x and y as computed by the function
    frexp().

    If x and y lie within this interval, they are considered
    approximately equal and the function returns 0. Otherwise if
    x < y, the function returns -1, or if x > y, the function
    returns +1.

    The implementation is based on the package fcmp by
    T.C. Belding."
  (pmnil
   (cffi:foreign-funcall
    "gsl_fcmp"
    :double x
    :double y
    :double epsilon
    :double)))
