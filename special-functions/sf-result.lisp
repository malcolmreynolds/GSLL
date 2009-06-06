;; CFFI-Grovel definitions for unix systems.
;; Liam Healy
;; Time-stamp: <2009-06-06 10:54:09EDT sf-result.lisp>

#+linux
(define "_GNU_SOURCE")

(in-package :gsl)

(include "gsl/gsl_sf_result.h")

;;; Results from special functions with value and error estimate.
;;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
(cstruct sf-result "gsl_sf_result"
  (val "val" :type :double)
  (err "err" :type :double))

;;; Results from special functions with value, error estimate
;;; and a scaling exponent e10, such that the value is val*10^e10.
;;; file:///usr/share/doc/gsl-ref-html/gsl-ref_7.html#SEC61
(cstruct sf-result-e10 "gsl_sf_result_e10"
  (val "val" :type :double)
  (err "err" :type :double)
  (e10 "e10" :type :int))
