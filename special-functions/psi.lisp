;********************************************************
; file:        psi.lisp                                  
; description: Psi (digamma) functions                   
; date:        Mon May  1 2006 - 22:11                   
; author:      Liam M. Healy                             
; modified:    Mon May  1 2006 - 22:25
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Digamma Function
;;;;****************************************************************************

(defgeneric psi (x)
  (:documentation "The psi, or digamma, function."))

(defun-gsl psi ((n :int))
  "gsl_sf_psi_int_e"
  :method ((n fixnum))
  :documentation "Domain: n integer, n > 0."
  :return (sf-result))

(defun-gsl psi ((x :double))
  "gsl_sf_psi_e"
  :method ((x double-float))
  :documentation "Domain: x /= 0.0, -1.0, -2.0, ..."
  :return (sf-result))

(defun-gsl psi-1piy ((x :double))
  "gsl_sf_psi_1piy_e"
  :documentation "The real part of the digamma function
  on the line @math{1+i y}, @math{\Re[\psi(1 + i y)]}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Trigamma Function
;;;;****************************************************************************

(defgeneric psi-1 (x)
  (:documentation "The Trigamma function."))

(defun-gsl psi-1 ((n :int))
  "gsl_sf_psi_1_int_e"
  :method ((n fixnum))
  :documentation "Domain: n integer, n > 0."
  :return (sf-result))

(defun-gsl psi-1 ((x :double))
  "gsl_sf_psi_1_e"
  :method ((x double-float))
  :documentation "Domain: x /= 0.0, -1.0, -2.0, ..."
  :return (sf-result))

;;;;****************************************************************************
;;;; Polygamma
;;;;****************************************************************************

(defun-gsl psi-n ((m :int) (x :double))
  "gsl_sf_psi_n_e"
  :documentation "The polygamma function @math{\psi^@{(m)@}(x)} for
   @math{m >= 0}, @math{x > 0}."
  :return (sf-result))

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test psi
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.125611766843d+01" (PSI 4))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.125611766843d+01" (PSI 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.283822955737d+00" (PSI-1 4))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "0.283822955737d+00" (PSI-1 4.0d0))
  (LISP-UNIT:ASSERT-FIRST-FP-EQUAL "-0.800397322451d-01" (PSI-N 2 4.0d0)))
