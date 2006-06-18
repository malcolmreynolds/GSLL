;********************************************************
; file:        psi.lisp                                  
; description: Psi (digamma) functions                   
; date:        Mon May  1 2006 - 22:11                   
; author:      Liam M. Healy                             
; modified:    Sat Jun 17 2006 - 22:30
;********************************************************
;;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Digamma Function
;;;;****************************************************************************

(defgeneric psi (x)
  (:documentation "The psi, or digamma, function."))

(defun-gsl psi  ((n fixnum))
  "gsl_sf_psi_int_e" ((n :int) (ret sf-result))
  :type :method
  :export t
  :documentation "Domain: n integer, n > 0.")

(defun-gsl psi ((x double-float))
  "gsl_sf_psi_e" ((x :double) (ret sf-result))
  :type :method 
  :documentation "Domain: x /= 0.0, -1.0, -2.0, ...")

(defun-gsl psi-1+iy (x)
  "gsl_sf_psi_1piy_e" ((x :double) (ret sf-result))
  :documentation "The real part of the digamma function
  on the line @math{1+i y}, @math{\Re[\psi(1 + i y)]}.")

;;;;****************************************************************************
;;;; Trigamma Function
;;;;****************************************************************************

(defgeneric psi-1 (x)
  (:documentation "The Trigamma function."))

(defun-gsl psi-1 ((n fixnum))
  "gsl_sf_psi_1_int_e" ((n :int) (ret sf-result))
  :type :method 
  :documentation "Domain: n integer, n > 0.")

(defun-gsl psi-1 ((x double-float))
  "gsl_sf_psi_1_e" ((x :double) (ret sf-result))
  :type :method
  :documentation "Domain: x /= 0.0, -1.0, -2.0, ...")

;;;;****************************************************************************
;;;; Polygamma
;;;;****************************************************************************

(defun-gsl psi-n (m x)
  "gsl_sf_psi_n_e" ((m :int) (x :double) (ret sf-result))
  :documentation "The polygamma function @math{\psi^@{(m)@}(x)} for
   @math{m >= 0}, @math{x > 0}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test psi
  (lisp-unit:assert-first-fp-equal "0.125611766843d+01" (psi 4))
  (lisp-unit:assert-first-fp-equal "0.125611766843d+01" (psi 4.0d0))
  (lisp-unit:assert-first-fp-equal "0.714591515374d+00" (psi-1+iy 2.0d0))
  (lisp-unit:assert-first-fp-equal "0.283822955737d+00" (psi-1 4))
  (lisp-unit:assert-first-fp-equal "0.283822955737d+00" (psi-1 4.0d0))
  (lisp-unit:assert-first-fp-equal "-0.800397322451d-01" (psi-n 2 4.0d0)))

