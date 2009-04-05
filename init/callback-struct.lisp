;; GSL structures for holding functions
;; Liam Healy 2009-04-04 22:15:56EDT callback-struct.lisp
;; Time-stamp: <2009-04-04 22:34:29EDT callback-struct.lisp>
;; $Id: $

(in-package :gsl)

;;; Only the function
(cffi:defcstruct fnstruct
  "Passing functions to GSL."
  ;; see /usr/include/gsl/gsl_math.h
  (function :pointer)
  (parameters :pointer))

;;; The function and its derivative(s)
(cffi:defcstruct fnstruct-fdf
  ;; See /usr/include/gsl/gsl_math.h
  "The definition of a function and its derivative for root finding in GSL."
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (parameters :pointer))

;;; The function and a dimension
(cffi:defcstruct fnstruct-dimension
  ;; See /usr/include/gsl/gsl_multiroots.h
  ;; or  /usr/include/gsl/gsl_monte.h
  (function :pointer)
  (dimension sizet)
  (parameters :pointer))

;;; The function, dimension, and derivatives
(cffi:defcstruct fnstruct-dimension-fdf
  ;; See /usr/include/gsl/gsl_multiroots.h
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (dimension sizet)
  (parameters :pointer))

;;; The function and two dimensions (for nonlinear fit).
(cffi:defcstruct fnstruct-fit
  ;; See gsl_multifit_function in /usr/include/gsl/gsl_multifit_nlin.h
  (function :pointer)
  (number-of-observations sizet)
  (number-of-parameters sizet)
  (parameters :pointer))

;;; The function, two dimensions, and derivatives (for nonlinear fit)
(cffi:defcstruct fnstruct-fit-fdf
  (function :pointer)
  (df :pointer)
  (fdf :pointer)
  (number-of-observations sizet)
  (number-of-parameters sizet)
  (parameters :pointer))
