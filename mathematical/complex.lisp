;; Functions of complex numbers
;; Liam Healy 2009-01-13 21:19:38EST complex.lisp
;; Time-stamp: <2009-01-15 14:14:07EST complex.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;; Properties
;;;;****************************************************************************

(defmfun argument (number)
  "gsl_complex_arg"
  ((number complex-double-c))
  :c-return :double
  :documentation
  "The angle of the complex number.")

(defmfun modulus (number)
  "gsl_complex_abs"
  ((number complex-double-c))
  :c-return :double
  :documentation "The magnitude, or modulus of the complex number.")

(defmfun modulus2 (number)
  "gsl_complex_abs2"
  ((number complex-double-c))
  :c-return :double
  :documentation "The magnitude squared of the complex number.")

(defmfun log-modulus (number)
  "gsl_complex_logabs"
  ((number complex-double-c))
  :c-return :double
  :documentation "The logarithm of the magnitude of the complex number.")

;;;;****************************************************************************
;;; Complex arithmetic
;;;;****************************************************************************

;;; The GSL functions provided duplicate what CL has built-in.

