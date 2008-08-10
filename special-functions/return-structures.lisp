;; Structures returned by special functions.
;; Liam Healy, Mon Jan  1 2007 - 11:35
;; Time-stamp: <2008-08-10 17:47:07EDT return-structures.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Result from special functions
;;;;****************************************************************************

(cffi:defcstruct sf-result
  "Results from special functions with value and error estimate."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
  (val :double)
  (err :double))

(cffi:defcstruct sf-result-e10
  "Results from special functions with value, error estimate
   and a scaling exponent e10, such that the value is val*10^e10."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
  (val :double)
  (err :double)
  (e10 :int))

(cffi:defcenum sf-mode
  "Numerical precision modes with which to calculate special functions."
  ;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC62
  :double
  :single
  :approx)

(defun val (sf-result &optional (type 'sf-result))
  (cffi:foreign-slot-value sf-result type 'val))

(defun err (sf-result &optional (type 'sf-result))
  (cffi:foreign-slot-value sf-result type 'err))

(defun e10 (sf-result)
  (cffi:foreign-slot-value sf-result 'sf-result-e10 'e10))

