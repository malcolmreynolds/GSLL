;; GSL structures for holding functions
;; Liam Healy 2009-04-04 22:15:56EDT callback-struct.lisp
;; Time-stamp: <2009-08-26 21:26:02EDT callback-struct.lisp>

(in-package :gsl)

#+linux
(define "_GNU_SOURCE")

;;; When installed through Mac Ports, GSL .h files will be found
;;; in /opt/local/include.
#+darwin
(cc-flags "-I/opt/local/include/")

(include "gsl/gsl_math.h")

;;; Only the function
;;; also in /usr/include/gsl/gsl_ntuple.h as gsl_ntuple_select_fn and
;;; gsl_ntuple_value_fn.
(cstruct fnstruct "gsl_function"
  (function "function" :type :pointer)
  (parameters "params" :type :pointer))

;;; The function and its derivative(s)
(cstruct fnstruct-fdf "gsl_function_fdf"
  (function "f" :type :pointer)
  (df "df" :type :pointer)
  (fdf "fdf" :type :pointer)
  (parameters "params" :type :pointer))

(include "gsl/gsl_multiroots.h") 

;;; The function and a dimension
;;; also defined in /usr/include/gsl/gsl_monte.h
(cstruct fnstruct-dimension "gsl_multiroot_function"
  (function "f" :type :pointer)
  (dimension "n" :type sizet)
  (parameters "params" :type :pointer))

;;; The function, dimension, and derivatives
(cstruct fnstruct-dimension-fdf "gsl_multiroot_function_fdf"
  (function "f" :type :pointer)
  (df "df" :type :pointer)
  (fdf "fdf" :type :pointer)
  (dimension "n" :type sizet)
  (parameters "params" :type :pointer))

(include "gsl/gsl_multifit_nlin.h") 

;;; The function and two dimensions (for nonlinear fit).
(cstruct fnstruct-fit "gsl_multifit_function"
  (function "f" :type :pointer)
  (number-of-observations "n" :type sizet)
  (number-of-parameters "p" :type sizet)
  (parameters "params" :type :pointer))

;;; The function, two dimensions, and derivatives (for nonlinear fit)
(cstruct fnstruct-fit-fdf "gsl_multifit_function_fdf"
  (function "f" :type :pointer)
  (df "df" :type :pointer)
  (fdf "fdf" :type :pointer)
  (number-of-observations "n" :type sizet)
  (number-of-parameters "p" :type sizet)
  (parameters "params" :type :pointer))
