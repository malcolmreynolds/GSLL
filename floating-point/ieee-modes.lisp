;; IEEE 754 Modes and masks
;; Liam Healy 2008-01-29 21:35:50EST ieee-modes.lisp
;; Time-stamp: <2008-02-17 18:36:37EST ieee-modes.lisp>
;; $Id: $

(in-package :gsl)

(cffi:defcenum ieee-types
  "IEEE 754 types, from /usr/include/gsl/gsl_ieee_utils.h"
  (:NAN 1) :INF :NORMAL :DENORMAL :ZERO)

(cffi:defcenum ieee-precisions
  "IEEE 754 precisions, from /usr/include/gsl/gsl_ieee_utils.h"
  (:SINGLE-PRECISION 1) :DOUBLE-PRECISION :EXTENDED-PRECISION)

(cffi:defcenum ieee-rounding
  "IEEE 754 rounding, from /usr/include/gsl/gsl_ieee_utils.h"
  (:TO-NEAREST 1) :DOWN :UP :TO-ZERO)

(cffi:defcenum ieee-mask
  "IEEE 754 mask, from /usr/include/gsl/gsl_ieee_utils.h"
  (:INVALID 1) (:DENORMALIZED 2) (:DIVISION-BY-ZERO 4)
  (:OVERFLOW 8) (:UNDERFLOW 16) (:ALL 31) (:INEXACT 32))

#+ieee-floating-point
(defmfun set-floating-point-modes (precision rounding exception-mask)
  "gsl_ieee_set_mode"
  (((cffi:foreign-enum-value 'ieee-precisions precision) :int)
   ((cffi:foreign-enum-value 'ieee-rounding rounding) :int)
   ((cffi:foreign-enum-value 'ieee-mask exception-mask) :int))
  :documentation
  "Set the IEEE 754 precision, rounding mode, and exception mask.")
