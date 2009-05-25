;; IEEE 754 Modes and masks
;; Liam Healy 2008-01-29 21:35:50EST ieee-modes.lisp
;; Time-stamp: <2009-05-25 14:37:57EDT ieee-modes.lisp>
;; $Id$

(in-package :gsl)

#+ieee-floating-point
(defmfun set-floating-point-modes (precision rounding exception-mask)
  "gsl_ieee_set_mode"
  (((cffi:foreign-enum-value 'ieee-precisions precision) :int)
   ((cffi:foreign-enum-value 'ieee-rounding rounding) :int)
   ((cffi:foreign-enum-value 'ieee-mask exception-mask) :int))
  :documentation
  "Set the IEEE 754 precision, rounding mode, and exception mask.")
